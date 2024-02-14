"""

http://uon.sdsu.edu/enghydro/engineering_hydrology_09.php#muskingum
Ponce -- Muskingum routing example
single reach
"""

import os

import flopy
import numpy as np
import pytest

from framework import TestFramework

# data from ponce Table 9-1
# time(d) coi2(m4/s) c1i1(m3/s) c2i1(m3/s) outflow(m3/s)
ponce_data = """0  352.0  0  0  0  352.0
1  587.0  76.6  107.1  199.0  382.7
2  1353.0  176.5  178.6  216.3  571.4
3  2725.0  355.4  411.8  323.0  1090.2
4  4408.5  575.0  829.4  616.2  2020.6
5  5987.0  780.9  1341.7  1142.1  3264.7
6  6704.0  874.4  1822.1  1845.3  4541.8
7  6951.0  906.7  2040.3  2567.1  5514.1
8  6839.0  892.0  2115.5  3116.7  6124.2
9  6207.0  809.6  2081.5  3461.5  6352.6
10  5346.0  697.3  1889.1  3590.6  6177.0
11  4560.0  594.8  1627.0  3491.4  5713.2
12  3861.5  503.7  1387.8  3229.2  5120.7
13  3007.0  392.2  1175.2  2894.3  4461.7
14  2357.5  307.5  915.2  2521.8  3744.5
15  1779.0  232.0  717.5  2116.5  3066.0
16  1405.0  183.3  541.4  1733.0  2457.7
17  1123.0  146.5  427.6  1389.1  1963.2
18  952.5  124.2  341.8  1109.6  1575.6
19  730.0  95.2  289.9  890.6  1275.7
20  605.0  78.9  222.2  721.0  1022.1
21  514.0  67.1  184.1  577.7  828.9
22  422.0  55.1  156.4  468.5  680.0
23  352.0  45.9  128.4  384.4  558.7
24  352.0  45.9  107.1  315.8  468.8
25  352.0  45.9  107.1  265.0  418.0"""


def get_ponce_data():
    qinflow = []
    qoutflow = []
    time_days = []
    for line in ponce_data.split("\n"):
        itime, inflow, c0, c1, c2, outflow = line.strip().split("  ")
        time_days.append(float(itime))
        qinflow.append(float(inflow))
        qoutflow.append(float(outflow))
    return time_days, qinflow, qoutflow


cases = [
    "ponce01",
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
    nper = 25
    tdis_rc = [(1.0, 1, 1.0) for ispd in range(nper)]
    tdis = flopy.mf6.ModflowTdis(sim, nper=nper, perioddata=tdis_rc)
    ems = flopy.mf6.ModflowEms(sim)
    swf = flopy.mf6.ModflowSwf(sim, modelname=name, save_flows=True)

    vertices = None
    cell2d = None
    nvert = None

    nodes = 1
    channel_length = 500.0  # miles
    channel_length = channel_length * 5280.0 / 3.2808  # meters
    reach_length = channel_length * 5280.0 / nodes

    k_coef = 2.0  # days
    x_coef = 0.1  # dimensionless

    time_days, qinflow, qoutflow = get_ponce_data()

    disl = flopy.mf6.ModflowSwfdisl(
        swf,
        nodes=nodes,
        nvert=nvert,
        reach_length=reach_length,
        reach_bottom=0.0,
        toreach=[
            (-1,)
        ],  # (-1,) gives 0 in one-based, which means outflow cell
        idomain=1,
        vertices=vertices,
        cell2d=cell2d,
    )

    # note: for specifying reach number, use fortran indexing!
    fname = f"{name}.mmr.obs.csv"
    mmr_obs = {
        fname: [
            ("OUTFLOW1", "EXT-OUTFLOW", 1),
        ],
        "digits": 10,
    }

    mmr = flopy.mf6.ModflowSwfmmr(
        swf,
        print_flows=True,
        observations=mmr_obs,
        iseg_order=list(range(nodes)),
        qoutflow0=qinflow[0],
        k_coef=k_coef,
        x_coef=x_coef,
    )

    # output control
    oc = flopy.mf6.ModflowSwfoc(
        swf,
        qoutflow_filerecord=f"{name}.qoutflow",
        budget_filerecord=f"{name}.bud",
        saverecord=[
            ("QOUTFLOW", "ALL"),
            ("BUDGET", "ALL"),
        ],
        printrecord=[
            ("BUDGET", "ALL"),
        ],
    )

    inflow = qinflow[1:]
    flw_spd = {ispd: [[0, inflow[ispd]]] for ispd in range(nper)}
    flw = flopy.mf6.ModflowSwfflw(
        swf,
        print_input=True,
        print_flows=True,
        stress_period_data=flw_spd,
    )

    return sim, None


def check_output(idx, test):
    print("evaluating model...")

    # get back the ponce data for comparison
    time_days, qinflow, qextoutflow = get_ponce_data()

    # read the observation output
    name = cases[idx]
    fpth = os.path.join(test.workspace, f"{name}.mmr.obs.csv")
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")

    # read qoutflow file
    fpth = os.path.join(test.workspace, f"{name}.qoutflow")
    qobj = flopy.utils.HeadFile(fpth, precision="double", text="QOUTFLOW")
    qoutflow = qobj.get_alldata()

    # compare output with known result
    time_days = time_days[1:]
    qextoutflow = -np.array(qextoutflow[1:])
    atol = 0.06
    success = np.allclose(qextoutflow, obsvals["OUTFLOW1"], atol=atol)
    if not success:
        for i, t in enumerate(time_days):
            qa = qextoutflow[i]
            qs = obsvals["OUTFLOW1"][i]
            d = qa - qs
            if i == 0:
                maxdiff = d
            else:
                if abs(d) > maxdiff:
                    maxdiff = abs(d)
            print(t, qa, qs, d)
        print(f"maximum difference is {maxdiff}")
    assert success

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
