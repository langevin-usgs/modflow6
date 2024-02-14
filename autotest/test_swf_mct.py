"""

The purpose of this test is to recreate the rectangular
cross section simulations reported by Todini (2007) using the
SWF MCT Package.  The model consists of a 50-reach stream
with a specified inflow.  The simulated outflow from the last
reach (the maximum rate) is compared with the maximum rate
reported by Todini (2007).  The test uses the 5 different slope
values in Table 2 and the reported 5 different Qmax values to
test the SWF MCT implementation.

"""

import os

import flopy
import numpy as np
import pytest

from framework import TestFramework

cases = [f"swf-mct01{a}" for a in ["a", "b", "c", "d", "e"]]

# answers from MCT paper for rectangular cross section simulation with varying slopes
qmax_answer = [894.68, 879.10, 819.78, 669.53, 423.11]
slope_test = [0.002, 0.001, 0.0005, 0.00025, 0.0001]


def get_inflow_hydrograph(Qbase, Qpeak, Tp, beta, t):
    return Qbase + (Qpeak - Qbase) * (t / Tp * np.exp(1 - t / Tp)) ** beta


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

    dt = 1800.0  # seconds
    perlen = 345600.0
    total_time = perlen
    nstp = int(total_time / dt)

    beta = 16
    Qbase = 100  # m^3/s
    Qpeak = 900  # m^3/s
    Tp = 24 * 60 * 60  # seconds
    times = np.arange(0, total_time + dt, dt)
    inflow_hydrograph = get_inflow_hydrograph(Qbase, Qpeak, Tp, beta, times)

    tdis = flopy.mf6.ModflowTdis(
        sim, nper=1, perioddata=[(perlen, nstp, 1.0)], time_units="seconds"
    )
    ems = flopy.mf6.ModflowEms(sim)
    swf = flopy.mf6.ModflowSwf(sim, modelname=name, save_flows=True)

    dx = 2000.0
    nreach = 50
    total_length = dx * nreach
    vertices = []
    vertices = [[j, j * dx, 0.0, 0.0] for j in range(nreach + 1)]
    cell2d = []
    for j in range(nreach):
        cell2d.append([j, 0.5, 2, j, j + 1])
    toreach = [j + 1 for j in range(nreach - 1)] + [-1]
    nodes = len(cell2d)
    nvert = len(vertices)

    disl = flopy.mf6.ModflowSwfdisl(
        swf,
        nodes=nodes,
        nvert=nvert,
        reach_length=dx,
        reach_bottom=0.0,
        toreach=toreach,  # -1 gives 0 in one-based, which means outflow cell
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

    mct = flopy.mf6.ModflowSwfmct(
        swf,
        observations=mmr_obs,
        print_flows=True,
        save_flows=True,
        icalc_order=list(range(nodes)),
        qoutflow0=inflow_hydrograph[0],
        width=50.0,
        manningsn=0.035,
        elevation=0.0,
        slope=slope_test[idx],
        idcxs=0,
    )

    xfraction = [0.0, 0.0, 1.0, 1.0]
    height = [100.0, 0.0, 0.0, 100.0]
    mannfraction = [1.0, 1.0, 1.0, 1.0]
    cxsdata = list(zip(xfraction, height, mannfraction))
    cxs = flopy.mf6.ModflowSwfcxs(
        swf,
        nsections=1,
        npoints=4,
        packagedata=[(0, 4)],
        crosssectiondata=cxsdata,
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

    # Create flw package with time series input
    flw = flopy.mf6.ModflowSwfflw(
        swf,
        maxbound=1,
        print_input=True,
        print_flows=True,
        stress_period_data=[(0, "inflow")],
    )
    fname = f"{name}.flw.ts"
    flw.ts.initialize(
        filename=fname,
        timeseries=list(zip(times, inflow_hydrograph)),
        time_series_namerecord=["inflow"],
        interpolation_methodrecord=["linearend"],
    )

    return sim, None


def check_output(idx, test):
    print("evaluating model...")

    # read the observation output
    name = cases[idx]
    fpth = os.path.join(test.workspace, f"{name}.mmr.obs.csv")
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")
    qoutflow = -obsvals["OUTFLOW"]
    qms = qoutflow.max()
    qma = qmax_answer[idx]
    d = abs(qms - qma)
    print(f"Outflow max mf6 ({qms}) and reported ({qma}); diff = {d}")
    dtol = 0.4
    assert (
        d < dtol
    ), f"Sim and reported max outflow too different; diff {d} > dtol {dtol}."

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