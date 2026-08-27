from __future__ import annotations
import argparse
from pathlib import Path
import sys

# Support both recommended ``python -m carpet`` and direct execution from an
# IDE (``python carpet/__main__.py``).  Relative imports only work in the first
# case, so establish the project root as an import location in the second.
if __package__:
    from .io import read_input, read_properties
    from .model import CarpetModel
else:
    project_root = Path(__file__).resolve().parent.parent
    sys.path.insert(0, str(project_root))
    from carpet.io import read_input, read_properties
    from carpet.model import CarpetModel


PROJECT_ROOT = Path(__file__).resolve().parent.parent


def main():
    ap=argparse.ArgumentParser(description="CARPET two-phase pipe simulation")
    ap.add_argument("--input", default=str(PROJECT_ROOT / "CARPET_01.txt"))
    ap.add_argument("--properties", default=str(PROJECT_ROOT / "CARPET_zagal_01.txt"))
    ap.add_argument("--output", default=str(PROJECT_ROOT / "results"))
    ap.add_argument("--cells",type=int,default=100)
    ap.add_argument("--until",type=float)
    ap.add_argument("--dt",type=float)
    a=ap.parse_args()
    model=CarpetModel(read_input(a.input),read_properties(a.properties),a.cells)
    print(f"Запуск: {a.cells} комірок; результат: {model.run(a.output,a.until,a.dt)}")


if __name__ == "__main__":
    main()
