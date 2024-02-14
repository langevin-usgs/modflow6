"""

Simple 4 segment network with 5 vertices


zero-based diagram below
v1
o
 |
  |
   |  s0
    |
     |
      |
o------o------o------o
v0     v2     v3     v4
    s1     s2     s3

ia  ja
0   0 2
2   1 2
4   2 0 1 3
8   3 2
10

"""

import os

import flopy
import numpy as np
import pytest

from framework import TestFramework

cases = [
    "swf-mmr01",
]


def build_models(idx, test):

    sim_ws = test.workspace
    name = cases[idx]
    sim = flopy.mf6.MFSimulation(
        sim_name=name,
        version="mf6",
        exe_name="mf6",
        sim_ws=sim_ws,
        memory_print_option="all",
    )

    tdis = flopy.mf6.ModflowTdis(sim)
    ems = flopy.mf6.ModflowEms(sim)
    swf = flopy.mf6.ModflowSwf(sim, modelname=name, save_flows=True)

    vertices = [
        [0, 0.0, 0.0, 0.0],
        [1, 0.0, 1.0, 0.0],
        [2, 1.0, 0.0, 0.0],
        [3, 2.0, 0.0, 0.0],
        [4, 3.0, 0.0, 0.0],
    ]
    # icell1d fdc ncvert icvert
    cell2d = [
        [0, 0.5, 2, 1, 2],
        [1, 0.5, 2, 0, 2],
        [2, 0.5, 2, 2, 3],
        [3, 0.5, 2, 3, 4],
    ]

    nodes = len(cell2d)
    nvert = len(vertices)

    disl = flopy.mf6.ModflowSwfdisl(
        swf,
        nodes=nodes,
        nvert=nvert,
        reach_length=1000.0,
        reach_bottom=0.0,
        toreach=[
            2,
            2,
            3,
            -1,
        ],  # -1 gives 0 in one-based, which means outflow cell
        idomain=1,
        vertices=vertices,
        cell2d=cell2d,
    )

    # note: for specifying zero-based reach number, put reach number in tuple
    fname = f"{name}.mmr.obs.csv"
    mmr_obs = {
        fname: [
            ("OUTFLOW", "EXT-OUTFLOW", (nodes - 1,)),
        ],
        "digits": 10,
    }

    mmr = flopy.mf6.ModflowSwfmmr(
        swf,
        observations=mmr_obs,
        print_flows=True,
        save_flows=True,
        iseg_order=list(range(nodes)),
        qoutflow0=0.0,
        k_coef=0.001,
        x_coef=0.2,
    )

    # output control
    oc = flopy.mf6.ModflowSwfoc(
        swf,
        budget_filerecord=f"{name}.bud",
        qoutflow_filerecord=f"{name}.qoutflow",
        saverecord=[
            ("QOUTFLOW", "ALL"),
            ("BUDGET", "ALL"),
        ],
        printrecord=[
            ("QOUTFLOW", "LAST"),
            ("BUDGET", "ALL"),
        ],
    )

    # Save to external binary file or into flw package depending on binary keyword
    binary = True
    flw_list = [(1, 1000), (2, 500.0)]  # one-based cell numbers here
    maxbound = len(flw_list)
    if binary:
        ra = np.array(flw_list, dtype=[("irch", "<i4"), ("q", "<f8")])
        ra.tofile(os.path.join(sim_ws, "flw0.bin"))
        flw_spd = {
            0: {
                "filename": "flw0.bin",
                "binary": True,
                "data": None,
            },
        }
    else:
        flw_spd = {0: flw_list}
    flw = flopy.mf6.ModflowSwfflw(
        swf,
        maxbound=maxbound,
        print_input=True,
        print_flows=True,
        stress_period_data=flw_spd,
    )

    return sim, None


def check_output(idx, test):
    print("evaluating model...")

    # read the observation output
    name = cases[idx]
    fpth = os.path.join(test.workspace, f"{name}.mmr.obs.csv")
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")

    # read the binary grid file
    fpth = os.path.join(test.workspace, f"{name}.disl.grb")
    grb = flopy.mf6.utils.MfGrdFile(fpth)
    ia = grb.ia
    ja = grb.ja
    assert ia.shape[0] == grb.nodes + 1, "ia in grb file is not correct size"

    # read qoutflow file
    fpth = os.path.join(test.workspace, f"{name}.qoutflow")
    qobj = flopy.utils.HeadFile(fpth, precision="double", text="QOUTFLOW")
    qoutflow = qobj.get_alldata()

    # read the budget file
    fpth = os.path.join(test.workspace, f"{name}.bud")
    budobj = flopy.utils.binaryfile.CellBudgetFile(fpth)
    flowja = budobj.get_data(text="FLOW-JA-FACE")
    qstorage = budobj.get_data(text="STORAGE")
    qflw = budobj.get_data(text="FLW")
    qextoutflow = budobj.get_data(text="EXT-OUTFLOW")
    qresidual = np.zeros(grb.nodes)

    # check budget terms
    for itime in range(len(flowja)):
        print(f"evaluating timestep {itime}")

        fja = flowja[itime].flatten()
        for n in range(grb.nodes):
            ipos = ia[n]
            qresidual[n] = fja[ipos]
        assert np.allclose(
            qresidual, 0.0
        ), "residual in flowja diagonal is not zero"

        for n in range(grb.nodes):
            qs = qstorage[itime].flatten()[n]
            if n + 1 in qflw[itime]["node"]:
                (idx,) = np.where(qflw[itime]["node"] == n + 1)
                idx = idx[0]
                qf = qflw[itime].flatten()["q"][idx]
            else:
                qf = 0.0
            qe = qextoutflow[itime].flatten()[n]
            qdiag = fja[ia[n]]
            print(f"{n=} {qs=} {qf=} {qe=} {qdiag=}")
            for ipos in range(ia[n] + 1, ia[n + 1]):
                j = ja[ipos]
                q = fja[ipos]
                print(f"  {ipos=} {j=} {q=}")

    return


@pytest.mark.parametrize("idx, name", enumerate(cases))
def test_mf6model(idx, name, function_tmpdir, targets):
    test = TestFramework(
        name=name,
        workspace=function_tmpdir,
        build=lambda t: build_models(idx, t),
        check=lambda t: check_output(idx, t),
        targets=targets,
    )
    test.run()
