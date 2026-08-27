from __future__ import annotations

from dataclasses import dataclass
import csv
import math
from pathlib import Path

from .io import InputData, PropertyTables
from .numerics import interp, interp2, solve_tridiagonal


def _safe_power(x: float, p: float) -> float:
    return max(x, 1e-30) ** p


@dataclass
class State:
    mass_flow: float
    front: float
    wall: list[list[float]]
    vapor_velocity: list[float]
    vapor_temperature: list[float]
    liquid_velocity: list[float]
    liquid_temperature: list[float]


class CarpetModel:
    """Restored implementation of the CARPET_MAN finite-difference model.

    The state layout follows the Fortran program, but named arrays replace COMMON
    blocks. RK4 advances mass flow, front, wall temperatures and phase variables.
    """

    def __init__(self, config: InputData, tables: PropertyTables, cells: int = 100):
        if cells < 3:
            raise ValueError("cells має бути не менше 3")
        self.c, self.p, self.n = config, tables, cells
        self.g = 9.81
        self.dz = config.length / cells
        self.dr = config.wall_thickness / 4
        self.area = math.pi * config.diameter**2 / 4
        self.perimeter = math.pi * config.diameter
        self.inlet_t = config.inlet_celsius + 273.15
        self.inlet_rho = self.prop("rho", self.inlet_t, config.pressure)
        self.inlet_nu = self.prop("nu", self.inlet_t, config.pressure)
        self.inlet_u = config.mass_flow / (self.area * self.inlet_rho)
        re = max(self.inlet_u * config.diameter / self.inlet_nu, 1e-12)
        tau = .0395 * self.inlet_rho * self.inlet_u**2 * re**-.25
        self.dp_theory = tau * self.perimeter * config.length / self.area
        aj = config.length / self.area
        self.ajo, self.ajt = config.ajo_factor * aj, config.ajt_factor * aj
        ksi = self.dp_theory * self.inlet_rho / config.mass_flow**2
        self.ksio, self.ksit = config.loss_factor * ksi, ksi
        self.dp = (self.ksio + self.ksit) / self.inlet_rho * config.mass_flow**2
        self.p_to = config.pressure - self.dp + self.ksio / self.inlet_rho * config.mass_flow**2
        tw = config.wall_temperature
        self.state = State(config.mass_flow, 0.0, [[tw] * (cells + 1) for _ in range(4)],
                           [0.0] * cells, [tw] * (cells + 1),
                           [0.0] * cells, [tw] * (cells + 1))
        self.pressure = [self.p_to] * (cells + 1)
        self.void = [0.0] * cells
        self.quality = [0.0] * cells
        self.regime = [0] * cells
        self.front_cells = 0
        self.front_times = [math.nan] * cells

    def prop(self, kind: str, temperature: float, pressure_pa: float) -> float:
        grid = {"rho": self.p.density, "cp": self.p.heat_capacity,
                "lambda": self.p.conductivity, "nu": self.p.viscosity}[kind]
        return interp2(temperature, pressure_pa * 1e-5, grid)

    def material(self, temperature: float):
        rows = self.p.steel if self.c.metal == 1 else self.p.copper
        xs = [r[0] for r in rows]
        return tuple(interp(temperature, xs, [r[k] for r in rows]) for k in (1, 2, 3))

    def valve(self, t: float) -> float:
        a, b = self.c.valve_time, self.c.valve_time + self.c.valve_duration
        if t <= a: return 1.0
        if t >= b: return 0.0
        return (b - t) / (b - a)

    def _copy(self, s: State) -> State:
        return State(s.mass_flow, s.front, [x[:] for x in s.wall],
                     s.vapor_velocity[:], s.vapor_temperature[:],
                     s.liquid_velocity[:], s.liquid_temperature[:])

    def _add(self, s: State, d: State, scale: float) -> State:
        q = self._copy(s)
        q.mass_flow += scale * d.mass_flow; q.front += scale * d.front
        for k in range(4):
            q.wall[k] = [a + scale*b for a, b in zip(q.wall[k], d.wall[k])]
        q.vapor_velocity = [a + scale*b for a, b in zip(q.vapor_velocity, d.vapor_velocity)]
        q.vapor_temperature = [a + scale*b for a, b in zip(q.vapor_temperature, d.vapor_temperature)]
        q.liquid_velocity = [a + scale*b for a, b in zip(q.liquid_velocity, d.liquid_velocity)]
        q.liquid_temperature = [a + scale*b for a, b in zip(q.liquid_temperature, d.liquid_temperature)]
        return q

    def _zero(self) -> State:
        return State(0., 0., [[0.] * (self.n + 1) for _ in range(4)],
                     [0.] * self.n, [0.] * (self.n + 1),
                     [0.] * self.n, [0.] * (self.n + 1))

    def _derivative(self, t: float, s: State) -> State:
        d = self._zero(); active = self.front_cells
        if active == 0:
            a = self.valve(t)
            d.mass_flow = (self.p_to + self.dp - (self.ksio + self.ksit*a) /
                           self.inlet_rho * abs(s.mass_flow)*s.mass_flow - self.p_to) / (self.ajo + self.ajt*a)
            return d

        rho_l = [self.prop("rho", s.liquid_temperature[j], self.pressure[j]) for j in range(active + 1)]
        rho_v = [self.prop("rho", s.vapor_temperature[j], self.pressure[j]) for j in range(active + 1)]
        nu_l = [self.prop("nu", s.liquid_temperature[j], self.pressure[j]) for j in range(active + 1)]
        nu_v = [self.prop("nu", s.vapor_temperature[j], self.pressure[j]) for j in range(active + 1)]
        cp_l = [self.prop("cp", s.liquid_temperature[j], self.pressure[j]) for j in range(active + 1)]
        cp_v = [self.prop("cp", s.vapor_temperature[j], self.pressure[j]) for j in range(active + 1)]
        lam_l = [self.prop("lambda", s.liquid_temperature[j], self.pressure[j]) for j in range(active + 1)]
        sat_t = [interp(self.pressure[j]*1e-5, self.p.saturation_p, self.p.saturation_t) for j in range(active + 1)]
        latent = [interp(self.pressure[j]*1e-5, self.p.saturation_p, self.p.latent_heat) for j in range(active + 1)]
        sigma = [interp(s.liquid_temperature[j], self.p.surface_t, self.p.surface) for j in range(active + 1)]
        ae, be, av, bv = ([0.]*active for _ in range(4))

        for j in range(active):
            rl = .5*(rho_l[j]+rho_l[j+1]); rv = .5*(rho_v[j]+rho_v[j+1])
            ul, uv = s.liquid_velocity[j], s.vapor_velocity[j]
            regime = max(self.regime[j], 1)
            flow = max(abs(s.mass_flow), 1e-4)
            if regime == 1:
                self.void[j] = self.quality[j] = 0.0
            else:
                den = rl*ul-rv*uv
                alpha = (rl*ul-flow/self.area)/den if abs(den) > 1e-20 else self.void[j]
                self.void[j] = min(max(alpha, 1e-8), 1-1e-8)
                self.quality[j] = min(max(rv*uv*self.area*self.void[j]/flow, 0.), 1.)
            alpha = self.void[j]
            rel = max(abs(ul)*self.c.diameter/max(.5*(nu_l[j]+nu_l[j+1]),1e-20), 1e-12)
            rev = max(abs(uv)*self.c.diameter/max(.5*(nu_v[j]+nu_v[j+1]),1e-20), 1e-12)
            mix = alpha*rv+(1-alpha)*rl
            tau_l = .0395*rl*ul*abs(ul)*rel**-.25
            tau_v = .0395*rv*uv*abs(uv)*rev**-.25
            ae[j] = 1/(rl*self.dz)
            be[j] = self.c.direction*self.g - tau_l*self.perimeter/(self.area*max(mix,1e-20))
            if regime >= 4:
                av[j] = 1/(rv*self.dz)
                bv[j] = self.c.direction*self.g - tau_v*self.perimeter/(self.area*max(rv,1e-20))

            pr_l = nu_l[j+1]*rho_l[j+1]*cp_l[j+1]/max(lam_l[j+1],1e-20)
            htc = .021*lam_l[j+1]/self.c.diameter*rel**.8*_safe_power(pr_l,.4)
            superheat = max(s.wall[0][j+1]-sat_t[j+1], 0.)
            if regime in (2, 3):
                # Chen-like nucleate/transition boiling correlation from source.
                htc += .00122*_safe_power(lam_l[j+1],.79)*_safe_power(cp_l[j+1],.45)*\
                       _safe_power(rho_l[j+1],.49)*self.g**.25/\
                       max(_safe_power(sigma[j+1]*rho_l[j+1]*nu_l[j+1],.5)*
                           _safe_power(latent[j+1]*rho_v[j+1],.24),1e-30)*\
                       _safe_power(superheat,.24)
            elif regime >= 4:
                mu_v = max(rho_v[j+1]*nu_v[j+1],1e-20)
                htc = 3.566*_safe_power(lam_l[j+1]**3*rho_v[j+1]*
                      max(rho_l[j+1]-rho_v[j+1],1e-9)*self.g*latent[j+1]/
                      max(mu_v*max(superheat,1e-6),1e-30),.25)*self.inlet_u**.4*\
                      max(s.front-self.dz*(j+.5),.05)**-.25
            q = htc*(s.wall[0][j+1]-s.liquid_temperature[j+1])
            liquid_area = self.area*max(1-alpha,1e-8)
            d.liquid_temperature[j+1] = q*self.perimeter/(cp_l[j+1]*liquid_area*rho_l[j+1])
            if regime >= 6:
                d.vapor_temperature[j+1] = htc*(s.wall[0][j+1]-s.vapor_temperature[j+1])*\
                    self.perimeter/(cp_v[j+1]*self.area*max(alpha,1e-8)*rho_v[j+1])

            # Radial/axial wall conduction. TW1 is intentionally fixed as in source.
            for k in (1, 2):
                rw, cw, kw = self.material(s.wall[k][j+1])
                diffusivity = kw/(rw*cw)
                r = self.c.diameter/2 + self.dr*k
                d.wall[k][j+1] = diffusivity*((s.wall[k+1][j+1]-s.wall[k-1][j+1])/(2*r*self.dr)
                    +(s.wall[k+1][j+1]-2*s.wall[k][j+1]+s.wall[k-1][j+1])/self.dr**2
                    +(s.wall[k][min(j+2,self.n)]-2*s.wall[k][j+1]+s.wall[k][j])/self.dz**2)

        # Pressure equation, represented directly as a tridiagonal system.
        if active == 1:
            pressures = [self.p_to+self.dp-self.ksio/self.inlet_rho*abs(s.mass_flow)*s.mass_flow, self.p_to]
        else:
            lower, diag, upper, rhs = [], [1.], [], [self.p_to+self.dp-self.ksio/self.inlet_rho*abs(s.mass_flow)*s.mass_flow]
            ml = [ae[j]*rho_l[j]*self.area*(1-self.void[j]) for j in range(active)]
            mv = [av[j]*rho_v[j]*self.area*self.void[j] for j in range(active)]
            bl = [be[j]*rho_l[j]*self.area*(1-self.void[j]) for j in range(active)]
            bv2 = [bv[j]*rho_v[j]*self.area*self.void[j] for j in range(active)]
            for j in range(1, active):
                left, right = ml[j-1]+mv[j-1], ml[j]+mv[j]
                lower.append(left); diag.append(-left-right); upper.append(right)
                rhs.append(-bl[j-1]-bv2[j-1]+bl[j]+bv2[j])
            lower.append(0.); diag.append(1.); upper = [0.] + upper + [0.]; rhs.append(self.p_to)
            pressures = solve_tridiagonal(lower, diag, upper[:active], rhs)
        self.pressure[:active+1] = pressures
        for j in range(active):
            d.liquid_velocity[j] = (pressures[j]-pressures[j+1])*ae[j]+be[j]
            d.vapor_velocity[j] = (pressures[j]-pressures[j+1])*av[j]+bv[j]
        mean_rho = sum(self.void[j]*rho_v[j]+(1-self.void[j])*rho_l[j] for j in range(active))/active
        d.front = s.mass_flow/(mean_rho*self.area)*(1-self.valve(t)) if s.front < self.c.length else 0.
        a = self.valve(t)
        d.mass_flow = (self.p_to+self.dp-(self.ksio+self.ksit*a)/self.inlet_rho*abs(s.mass_flow)*s.mass_flow-self.p_to)/(self.ajo+self.ajt*a)
        return d

    def _rk4(self, t: float, dt: float):
        s = self.state
        k1 = self._derivative(t, s)
        k2 = self._derivative(t+dt/2, self._add(s,k1,dt/2))
        k3 = self._derivative(t+dt/2, self._add(s,k2,dt/2))
        k4 = self._derivative(t+dt, self._add(s,k3,dt))
        q = self._copy(s)
        for d, w in ((k1,1),(k2,2),(k3,2),(k4,1)):
            q = self._add(q,d,dt*w/6)
        q.front = min(max(q.front,0.),self.c.length)
        self.state = q

    def _switch(self, t: float):
        target = min(self.n, int(self.state.front/self.dz)+1) if t > self.c.valve_time else 0
        while self.front_cells < target:
            j=self.front_cells; self.front_cells+=1; self.regime[j]=1; self.front_times[j]=t
        for j in range(self.front_cells):
            ts = interp(self.pressure[j+1]*1e-5, self.p.saturation_p, self.p.saturation_t)
            tl = self.state.liquid_temperature[j+1]
            # Ordered, reversible seven-regime state machine retained from Fortran.
            if self.regime[j] == 1 and tl >= ts: self.regime[j] = 2
            elif self.regime[j] == 2 and tl >= ts+2: self.regime[j] = 3
            elif self.regime[j] == 3 and tl >= ts+10: self.regime[j] = 4
            elif self.regime[j] == 4 and self.void[j] >= .35: self.regime[j] = 5
            elif self.regime[j] == 5 and self.void[j] >= .7: self.regime[j] = 6
            elif self.regime[j] == 6 and self.quality[j] >= .99: self.regime[j] = 7

    def run(self, output: str | Path = "results", until: float | None = None, dt: float | None = None):
        end, step = until if until is not None else self.c.end, dt if dt is not None else self.c.dt
        out = Path(output); out.mkdir(parents=True, exist_ok=True)
        fields = ["time","mass_flow","front","cell","wall_temperature","liquid_temperature",
                  "vapor_temperature","pressure","liquid_velocity","vapor_velocity","quality","void_fraction","regime"]
        with (out/"history.csv").open("w",newline="",encoding="utf-8") as f:
            wr=csv.writer(f); wr.writerow(fields)
            t=self.c.start; iteration=0
            while t <= end + step/2:
                if iteration % self.c.print_every == 0:
                    for j in sorted(set(min(self.n-1,x) for x in (9,29,49,69,89))):
                        wr.writerow([t,self.state.mass_flow,self.state.front,j+1,self.state.wall[3][j+1],
                          self.state.liquid_temperature[j+1],self.state.vapor_temperature[j+1],self.pressure[j+1],
                          self.state.liquid_velocity[j],self.state.vapor_velocity[j],self.quality[j],self.void[j],self.regime[j]])
                self._switch(t); self._rk4(t,step); t+=step; iteration+=1
        with (out/"front.csv").open("w",newline="",encoding="utf-8") as f:
            wr=csv.writer(f); wr.writerow(["cell","position","arrival_time","regime"])
            for j in range(self.n): wr.writerow([j+1,(j+1)*self.dz,self.front_times[j],self.regime[j]])
        return out
