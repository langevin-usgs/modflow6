"""

http://uon.sdsu.edu/the_thomas_problem_with_online_computation.html
Thomas routine example
500 miles
dx = 25 miles
sinusoidal inflow

"""

import os

import flopy
import numpy as np
import pytest

from framework import TestFramework

ponce_data = """0 0 50 50
1 3 51.441 50
2 6 55.709 50
3 9 62.64 50
4 12 71.967 50
5 15 83.332 50
6 18 96.299 50
7 21 110.368 50
8 24 125 50
9 27 139.632 50
10 30 153.701 50
11 33 166.668 50
12 36 178.033 50
13 39 187.36 50
14 42 194.291 50
15 45 198.559 50.002
16 48 200 50.006
17 51 198.559 50.018
18 54 194.291 50.049
19 57 187.36 50.121
20 60 178.033 50.275
21 63 166.668 50.575
22 66 153.701 51.119
23 69 139.632 52.034
24 72 125 53.476
25 75 110.368 55.616
26 78 96.299 58.627
27 81 83.332 62.656
28 84 71.967 67.811
29 87 62.64 74.142
30 90 55.709 81.625
31 93 51.441 90.165
32 96 50 99.589
33 99 50 109.662
34 102 50 120.091
35 105 50 130.549
36 108 50 140.685
37 111 50 150.146
38 114 50 158.595
39 117 50 165.722
40 120 50 171.266
41 123 50 175.021
42 126 50 176.847
43 129 50 176.676
44 132 50 174.517
45 135 50 170.453
46 138 50 164.641
47 141 50 157.305
48 144 50 148.724
49 147 50 139.223
50 150 50 129.156
51 153 50 118.886
52 156 50 108.765
53 159 50 99.11
54 162 50 90.185
55 165 50 82.184
56 168 50 75.223
57 171 50 69.342
58 174 50 64.515
59 177 50 60.662
60 180 50 57.669
61 183 50 55.403
62 186 50 53.732
63 189 50 52.528
64 192 50 51.68
65 195 50 51.097
66 198 50 50.704
67 201 50 50.444
68 204 50 50.275
69 207 50 50.168
70 210 50 50.101
71 213 50 50.06
72 216 50 50.035
73 219 50 50.02
74 222 50 50.012
75 225 50 50.007
76 228 50 50.004
77 231 50 50.002
78 234 50 50.001
79 237 50 50.001
80 240 50 50"""


def get_ponce_data():
    qinflow = []
    qoutflow = []
    time_days = []
    for line in ponce_data.split("\n"):
        itime, elapsed_time, inflow, outflow = line.strip().split(" ")
        time_days.append(float(elapsed_time))
        qinflow.append(float(inflow))
        qoutflow.append(float(outflow))
    return time_days, qinflow, qoutflow


cases = [
    "thomas01",
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
    delta_t = 60 * 60 * 3  # seconds = 3 hours
    nper = 80
    tdis_rc = [(delta_t, 1, 1.0) for ispd in range(nper)]
    tdis = flopy.mf6.ModflowTdis(
        sim, nper=nper, perioddata=tdis_rc, time_units="seconds"
    )
    ems = flopy.mf6.ModflowEms(sim)
    swf = flopy.mf6.ModflowSwf(sim, modelname=name, save_flows=True)

    vertices = None
    cell2d = None
    nvert = None

    nodes = 20  # can run 200,000 reaches in less than a second and 2 million reaches in 4 seconds
    channel_length = 500 * 5280.0  # meters
    reach_length = channel_length / nodes

    qp = 200  # cfs = peak discharge
    qb = 50  # cfs = base discharge
    qa = (qp + qb) / 2

    beta = 5.0 / 3.0
    rating_coefficient = 0.688
    depth_a = 2.0
    # solve for reference depth from rating equation
    # reference depth is the depth when flow is reference flow qa
    depth_a = (qa / rating_coefficient) ** (1 / beta)
    v_mean = qa / depth_a  # mean velocity at peak discharge
    wave_celerity = beta * v_mean  # 4.0 # m/s calculated as beta * v
    q0 = qa  # flow per unit width based on average discharge
    S0 = 1.0 / 5280.0  # channel bottom slope, 1 ft/mi converted to ft/ft

    print(f"{depth_a=} ft")
    print(f"{v_mean=} ft/s")
    print(f"{wave_celerity=} ft/s")

    k_coef = reach_length / wave_celerity  # seconds
    x_coef = 0.5 * (
        1 - q0 / S0 / wave_celerity / reach_length
    )  # dimensionless

    time_days, qinflow, qoutflow = get_ponce_data()

    toreach = [(irch,) for irch in range(1, nodes)] + [(-1,)]
    disl = flopy.mf6.ModflowSwfdisl(
        swf,
        length_units="feet",
        nodes=nodes,
        nvert=nvert,
        reach_length=reach_length,
        reach_bottom=0.0,
        toreach=toreach,  # (-1,) gives 0 in one-based, which means outflow cell
        idomain=1,
        vertices=vertices,
        cell2d=cell2d,
    )

    # note: for specifying reach number, use fortran indexing!
    fname = f"{name}.mmr.obs.csv"
    mmr_obs = {
        fname: [
            ("OUTFLOW", "EXT-OUTFLOW", (nodes - 1,)),
        ],
        "digits": 10,
    }

    mmr = flopy.mf6.ModflowSwfmmr(
        swf,
        print_flows=False,
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

    # read the binary grid file
    name = cases[idx]
    fpth = os.path.join(test.workspace, f"{name}.disl.grb")
    grb = flopy.mf6.utils.MfGrdFile(fpth)
    ia = grb.ia
    ja = grb.ja
    assert ia.shape[0] == grb.nodes + 1, "ia in grb file is not correct size"

    # read the observation output
    fpth = os.path.join(test.workspace, f"{name}.mmr.obs.csv")
    obsvals = np.genfromtxt(fpth, names=True, delimiter=",")

    # read qoutflow file
    fpth = os.path.join(test.workspace, f"{name}.qoutflow")
    qobj = flopy.utils.HeadFile(fpth, precision="double", text="QOUTFLOW")
    qoutflow = qobj.get_alldata()

    # compare output with known result
    time_days = time_days[1:]
    qextoutflow = -np.array(qextoutflow[1:])
    atol = 0.001
    success = np.allclose(qextoutflow, obsvals["OUTFLOW"], atol=atol)
    if not success:
        for i, t in enumerate(time_days):
            qa = qextoutflow[i]
            qs = obsvals["OUTFLOW"][i]
            d = qa - qs
            if i == 0:
                maxdiff = d
            else:
                if abs(d) > maxdiff:
                    maxdiff = abs(d)
            print(t, qa, qs, d)
        print(f"maximum difference is {maxdiff}")
    assert success

    # ensure obs compares with binary outflow
    atol = 1.0e-6
    for i, t in enumerate(time_days):
        qoutflow_obs = -obsvals["OUTFLOW"][i]
        qoutflow_bin = qoutflow[i].flatten()[grb.nodes - 1]
        d = qoutflow_obs - qoutflow_bin
        if i == 0:
            maxdiff = d
        else:
            if abs(d) > maxdiff:
                maxdiff = abs(d)
        print(t, qoutflow_obs, qoutflow_bin, d)
    print(f"maximum difference is {maxdiff}")
    assert maxdiff < atol, f"max diff {maxdiff} not less than atol {atol}"

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
