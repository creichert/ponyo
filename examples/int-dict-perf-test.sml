structure D = Ponyo.Int.Dict
structure M = Ponyo.Int.Map
structure P = PolyML.Profiling

val a = [
5208,
3429,
7558,
15449,
2132,
17655,
10884,
16937,
19195,
14545,
12582,
8818,
5232,
19247,
1734,
17259,
3277,
5686,
10764,
2984,
19267,
17667,
16909,
3303,
16242,
12279,
12215,
2669,
6725,
9350,
11710,
12625,
9876,
3861,
8238,
2617,
10358,
2536,
11013,
1392,
5840,
4651,
18120,
357,
1407,
3137,
16988,
15982,
13012,
17011,
9000,
573,
17596,
14070,
2073,
4361,
11116,
8858,
286,
18536,
7540,
5470,
9287,
18136,
12646,
5299,
7957,
17538,
1270,
1891,
8830,
10737,
1016,
12264,
2008,
670,
10441,
7764,
17325,
7148,
12726,
802,
19413,
5569,
5520,
12969,
14986,
5015,
18679,
5068,
18831,
4945,
5070,
5179,
8007,
12175,
9839,
5121,
13228,
3248,
12229,
131,
12629,
4434,
3614,
15527,
9092,
2570,
12781,
3866,
8171,
567,
5894,
5129,
10200,
17459,
14307,
3320,
6762,
18278,
16345,
12311,
5023,
2693,
8794,
16242,
17088,
13612,
14725,
8059,
4618,
12208,
15246,
8513,
7538,
1533,
4929,
4524,
7767,
624,
7836,
16089,
879,
7124,
4329,
11844,
15703,
19049,
9076,
1456,
7130,
11535,
17802,
16886,
16070,
18559,
3868,
10041,
5642,
12636,
18489,
8222,
11372,
11428,
11292,
11124,
16357,
15543,
1542,
17383,
14918,
14199,
17578,
5928,
2303,
8249,
2924,
14834,
15374,
7770,
14084,
3980,
14539,
6297,
2358,
4166,
12746,
12646,
5629,
11320,
16577,
4094,
19834,
17708,
9458,
9247,
13412,
16506,
8221,
17396,
2695,
11857,
1707,
8369,
3715,
19527,
14473,
13672,
14772,
18944,
8020,
16135,
12984,
9185,
17221,
3077,
16683,
18595,
18921,
14570,
3359,
6775,
11070,
11303,
8025,
17547,
4287,
4187,
19134,
5184,
17934,
2413,
8698,
15141,
3855,
16730,
12048,
5656,
16081,
1379,
2495,
3287,
13625,
901,
14976,
13691,
11200,
3873,
8643,
17108,
18194,
3697,
2542,
3201,
17954,
1980,
12892,
16989,
10566,
10909,
15895,
1508,
8231,
13882,
10191,
6637,
12093,
1710,
2573,
6124,
10760,
4755,
3808,
10312,
10077,
18999,
3981,
17746,
5908,
9068,
14582,
14843,
17406,
2048,
1285,
18881,
13669,
17969,
15078,
13666,
7947,
4419,
7013,
12909,
16965,
12049,
15775,
13624,
12721,
15147,
12964,
13265,
18522,
19855,
10454,
18457,
7476,
6464,
7264,
16765,
9890,
11484,
5649,
16371,
12124,
1079,
13343,
1961,
739,
13508,
17846,
17851,
14642,
2780,
17393,
14704,
6034,
14656,
19859,
18211,
10754,
17318,
4892,
7099,
14163,
10905,
8168,
9227,
13678,
18021,
17483,
4776,
10502,
18613,
15434,
6539,
14378,
16422,
10067,
12385,
12458,
9058,
17400,
3899,
8701,
7517,
5749,
11848,
11150,
3992,
8797,
11429,
18344,
1219,
13463,
12323,
3502,
8884,
14021,
4497,
8614,
19947,
6291,
8407,
15660,
16005,
8845,
11548,
2317,
3156,
6384,
8170,
10813,
8565,
13679,
17041,
14069,
16482,
15380,
11998,
14113,
10224,
17433,
2588,
6157,
10223,
16542,
2973,
3898,
14111,
807,
3872,
7558,
2185,
17715,
6575,
17814,
8383,
13967,
15030,
19877,
16216,
13810,
19605,
6446,
1521,
16154,
13554,
6903,
19614,
12911,
11985,
1481,
6164,
18883,
15673,
11428,
12015,
17975,
10266,
441,
13643,
15407,
10919,
16216,
16188,
3576,
16448,
10925,
19497,
9341,
3903,
17457,
11693,
1778,
18287,
7224,
8095,
7165,
16021,
7727,
16565,
13078,
6701,
17890,
17250,
19460,
19932,
13301,
17267,
7576,
17928,
8600,
3366,
112,
19676,
5960,
9281,
12442,
13913,
13206,
5403,
19262,
8203,
2373,
19377,
17158,
8764,
7735,
13292,
1758,
13686,
10666,
9789,
16813,
6424,
18392,
12861,
11019,
1360,
4050,
2587,
16442,
10751,
15987,
1882,
2713,
10661,
3935,
15758,
2871,
6094,
6649,
16803,
17576,
15595,
1873,
19654,
6119,
400,
14789,
895,
2578,
5321,
3752,
10052,
4911,
11133,
18706,
4336,
10613,
10207,
19853,
427,
13357,
2686,
14000,
19705,
4411,
4350,
11893,
18387,
7869,
19587,
19658,
9563,
5090,
7153,
1129,
15800,
12031,
10418,
16160,
19495,
11113,
803,
8200,
8328,
9752,
13586,
9361,
2227,
3340,
5528,
4619,
10344,
19181,
6190,
1026,
9680,
8541,
15972,
7229,
5393,
5200,
7217,
11009,
5716,
8111,
6416,
1533,
710,
11121,
7266,
12255,
5613,
4901,
9815,
18762,
2071,
7769,
7117,
2575,
12355,
9627,
13829,
18143,
1291,
6180,
4708,
13055,
4789,
3995,
2616,
24,
523,
6225,
5246,
7137,
3711,
15179,
3846,
19642,
14158,
17324,
13768,
7392,
1937,
2911,
2981,
502,
4448,
18657,
2954,
15346,
17306,
13026,
17477,
16480,
18381,
213,
17846,
19551,
12633,
11827,
4219,
17279,
5526,
19716,
15069,
16368,
6325,
14894,
9083,
16079,
15065,
2987,
10384,
6205,
6122,
9670,
13625,
4127,
6774,
4617,
3412,
10119,
13321,
12178,
340,
11080,
15284,
17212,
14670,
6663,
16668,
2076,
3143,
18873,
3095,
7018,
5348,
7648,
11330,
17696,
16830,
12259,
17366,
1091,
12333,
5503,
4605,
2733,
4882,
4821,
8176,
18745,
1865,
11841,
8726,
8289,
2888,
10381,
18901,
1815,
411,
7821,
9513,
1544,
757,
10023,
16911,
19898,
14453,
12201,
5240,
15814,
19561,
12452,
15613,
171,
16842,
12598,
1826,
11417,
17869,
18227,
15692,
18290,
17856,
2484,
17024,
5307,
19375,
5132,
14748,
6193,
13110,
7362,
18748,
1288,
13887,
19185,
10996,
18898,
19582,
16445,
5816,
13475,
7030,
16766,
18181,
14776,
4604,
19517,
17469,
12712,
2361,
2688,
13227,
13767,
13584,
9815,
18332,
16452,
1289,
6283,
19707,
650,
19305,
13028,
9833,
14895,
197,
4393,
19492,
5554,
6544,
19461,
10450,
19727,
19009,
6511,
15119,
13678,
8004,
5062,
5168,
4248,
3323,
5105,
3739,
8644,
8101,
7343,
9346,
17199,
1720,
5563,
5593,
11537,
11395,
3995,
7619,
13825,
10448,
17805,
12652,
10737,
10625,
13325,
16818,
1031,
14649,
1014,
9672,
16613,
14514,
13237,
19477,
3203,
10302,
6875,
14150,
6992,
9846,
18823,
8227,
3901,
1204,
17663,
16827,
1023,
12617,
13770,
992,
15041,
9116,
1152,
4866,
9718,
13943,
12520,
3070,
19439,
1548,
5074,
14471,
17921,
9423,
8015,
18717,
8997,
2929,
13656,
8159,
9070,
4596,
5547,
17173,
17484,
6508,
12539,
18784,
1378,
16041,
5589,
18001,
9084,
11145,
15443,
1569,
1021,
12180,
2438,
16213,
18221,
16659,
1851,
3010,
7705,
9558,
16715,
13024,
16167,
18250,
12394,
2110,
15730,
10426,
4636,
1342,
15613,
10014,
16013,
9179,
831,
10970,
1303,
699,
13488,
1052,
2806,
7922,
19919,
6945,
5770,
2689,
17507,
16907,
19105,
13230,
248,
15385,
18096,
10675,
114,
16815,
2722,
14419,
15821,
7753,
17144,
10883,
14986,
487,
12536,
15253,
11946,
2884,
1203,
8890,
16158,
14827,
14394,
5905,
13867,
11093,
17806,
7289,
10437,
19610,
3545,
15733,
374,
3853,
2217,
6563,
9898,
9383,
17569,
10147,
10932,
17741,
17659,
18836,
11746,
74,
5564,
14523,
10340,
13981,
5757,
5601,
10451,
10530,
17448,
18349,
19667,
11292,
16268,
19008,
7878,
10811,
14270,
11760,
8376,
14504,
9039,
14791,
10356,
4004,
10616,
11273,
16496,
10093,
16229,
10742,
6854,
1781,
1372,
810,
7350,
16160,
15002,
15809,
12796,
8138,
6307,
8475,
15616,
10689,
12423,
13949,
9332,
17216,
7207,
17028,
6564,
6090,
1602,
17372,
3099,
16517,
19961,
5122,
14732,
7515,
18537,
12198,
14859,
1859,
11133,
18588,
3545,
3049,
16680,
5666,
3100,
13279,
9651,
15910,
17264,
13216,
8334,
10288,
294,
8168,
15390,
11807,
4678,
12545,
17766,
14816,
3035,
10041,
6316,
14099,
7595,
12767,
8200,
3609,
10732,
6487,
4955,
5992,
17180,
2456,
9303,
16293,
13029,
7445,
12337,
11884,
16255,
12544,
1783,
70,
12233,
8268,
11498,
18488,
1179,
19924,
13802,
7030,
5494,
5789,
6789,
2195,
11344,
19426,
16959,
5714,
2925,
18111,
13283,
12647,
4540,
10363,
9656,
3685,
12978,
587,
9770,
14137,
14920,
11367,
19998,
6591,
1679,
10773,
10594,
19058,
11408,
18775,
15838,
7913,
1753,
13010,
11988,
2477,
4453,
3138,
15638,
2028,
4392,
13692,
11303,
6585,
13091,
8936,
2319,
13169,
19109,
10517,
4140,
12062,
17911,
17392,
10214,
16325,
8663,
14896,
8322,
2623,
15768,
19532,
14210,
13278,
13251,
16657,
4916,
459,
6359,
13150,
10258,
9345,
5542,
7612,
7483,
4800,
6272,
14544,
3336,
6146,
7640,
8259,
12754,
15782,
12728,
9675,
3902,
6289,
12430,
13773,
16044,
17281,
13651,
14121,
1366,
10210,
12252,
17179,
4897,
8140,
5422,
15906,
462,
13437,
15723,
11392,
3230,
14679,
8369,
14583,
8833,
7038,
19323,
14971,
6594,
7076,
4406,
11733,
7086,
9911,
18268,
18267,
12406,
7915,
17510,
9066,
16496,
5529,
19663,
14398,
7997,
14789,
18065,
3550,
17615,
507,
7079,
5081,
13469,
4259,
12117,
777,
5461,
1294,
5893,
15319,
15829,
4556,
15904,
18919,
14098,
14894,
4965,
3542,
18636,
11317,
2074,
14474,
10351,
16510,
9037,
13596,
13975,
6093,
11187,
11192,
2684,
5745,
11338,
11223,
18590,
13853,
18537,
6066,
12010,
3287,
978,
17945,
11799,
18968,
723,
15608,
5485,
11700,
280,
18741,
3460,
13963,
6544,
534,
625,
7065,
6368,
6979,
9939,
7550,
7053,
9431,
2969,
12362,
3588,
13478,
8153,
9531,
19386,
2165,
1136,
1734,
8252,
19340,
16152,
5463,
1955,
5177,
8907,
13896,
13407,
5974,
15331,
16166,
2807,
15532,
11738,
16980,
18172,
10236,
6413,
11737,
15749,
13228,
6289,
1749,
5981,
14794,
3075,
13237,
4803,
4157,
19291,
4658,
17194,
203,
19465,
11889,
14236,
2775,
12530,
9349,
9195,
13401,
14102,
18612,
2646,
2087,
12549,
16426,
17343,
14562,
18716,
19127,
12152,
11599,
13100,
7423,
6060,
1463,
16751,
55,
6076,
14885,
19904,
2717,
17380,
16484,
12655,
19651,
17554,
11517,
5718,
13340,
161,
18317,
18661,
12522,
5669,
7442,
2152,
11023,
10122,
7267,
19589,
15243,
11970,
11499,
13615,
8925,
3381,
15219,
16240,
5238,
15296,
12591,
17155,
1183,
10901,
1839,
11384,
541,
981,
16649,
213,
8870,
16311,
5988,
5921,
9086,
447,
11128,
12360,
14848,
18659,
1053,
832,
9022,
2715,
18959,
10660,
4208,
14738,
14291,
11195,
54,
8311,
7885,
12724,
18312,
514,
5909,
1007,
12955,
11376,
2286,
11044,
4582,
16587,
14912,
6681,
18405,
1549,
12342,
18027,
5156,
8438,
4020,
6971,
12160,
16352,
10963,
12077,
3254,
3544,
16055,
12132,
14788,
19735,
10438,
6617,
14244,
6462,
4193,
8698,
9794,
15737,
7126,
12802,
5195,
8774,
19833,
10707,
5048,
11052,
1950,
5375,
11965,
10931,
3100,
8590,
10027,
754,
16394,
6538,
1782,
17606,
2421,
4740,
1156,
11950,
10382,
300,
18703,
12011,
13524,
3440,
13275,
4027,
15610,
12764,
1562,
13175,
17705,
14637,
12480,
3520,
14730,
13411,
5484,
19588,
17234,
13815,
5946,
10281,
13977,
15575,
19357,
8186,
12205,
6903,
142,
12421,
10613,
12745,
15583,
2193,
3712,
299,
4945,
1572,
12779,
2039,
16755,
13950,
14777,
357,
3726,
5251,
2055,
6124,
3923,
15187,
19186,
13046,
17209,
14829,
10724,
9944,
11004,
19347,
18865,
18219,
16885,
17464,
9957,
1285,
18163,
12655,
4988,
2238,
14660,
18359,
18771,
5947,
16288,
10067,
15734,
3083,
18496,
2097,
15922,
3700,
4136,
18371,
2415,
4091,
3110,
5736,
8905,
4489,
18618,
5883,
9860,
2553,
17820,
16160,
19508,
16659,
18788,
13432,
9921,
4753,
13591,
9895,
19486,
5189,
15576,
14123,
3114,
15473,
19673,
9602,
19244,
19753,
4985,
15184,
9031,
1632,
220,
13607,
16597,
12953,
15806,
3464,
816,
812,
4959,
12951,
4436,
8326,
12570,
18018,
11987,
19384,
16569,
323,
13032,
19625,
19083,
4708,
4822,
10851,
4461,
2113,
11512,
9674,
12742,
6067,
10242,
350,
3393,
711,
19646,
11554,
15368,
13670,
1714,
3926,
1498,
472,
8108,
5200,
12850,
19087,
6853,
9311,
18847,
5318,
18738,
9363,
16677,
2301,
8522,
18134,
729,
12385,
16621,
15480,
18102,
1073,
1505,
13981,
2666,
10856,
7852,
12723,
13801,
3552,
4655,
8580,
3230,
13190,
14079,
14361,
14665,
19157,
19366,
14625,
10121,
1249,
11143,
8211,
19508,
16054,
10950,
13573,
8311,
10737,
3728,
4254,
17072,
8558,
10338,
13707,
13294,
3653,
1448,
84,
12163,
10436,
18507,
17076,
5021,
6301,
4821,
7285,
6406,
7698,
6184,
7995,
611,
19090,
13346,
13566,
6263,
7371,
499,
6391,
16981,
3796,
14995,
13636,
13509,
19588,
9618,
19800,
1772,
3609,
14969,
2591,
1194,
17976,
13338,
7778,
10410,
3368,
17951,
4548,
11155,
9101,
11978,
5395,
14360,
10140,
10520,
12514,
5056,
8383,
9824,
5378,
10127,
3751,
17019,
4178,
19592,
9226,
4576,
9960,
1073,
1913,
19540,
18278,
18098,
14766,
11034,
6586,
16426,
3577,
3375,
18703,
7657,
5108,
13108,
17218,
18185,
9981,
10707,
4743,
8559,
13904,
4641,
17258,
19240,
15864,
1797,
14284,
17156,
7803,
12038,
11669,
13990,
14581,
11077,
6074,
15699,
9332,
1332,
6443,
13864,
1673,
10902,
6649,
10668,
10155,
18132,
10729,
2372,
12988,
12484,
7968,
7384,
8846,
9726,
3174,
914,
11878,
5813,
502,
17151,
14301,
6036,
16054,
6666,
15985,
99,
12242,
10629,
1287,
3251,
7849,
9835,
14433,
5797,
18441,
627,
1950,
3769,
8340,
17865,
400,
12180,
18306,
19611,
16728,
523,
12871,
6598,
8124,
7128,
12539,
12057,
421,
9224,
8124,
8558,
6203,
4238,
5634,
703,
10955,
17357,
16394,
15136,
16485,
10361,
14080,
11661,
3079,
2128,
6918,
4234,
336,
1727,
4406,
5188,
17443,
14359,
19103,
5945,
5380,
14764,
18471,
6045,
1319,
4026,
8786,
7224,
13822,
16461,
729,
10426,
10665,
8220,
16725,
12111,
13806,
2908,
2267,
18087,
4981,
18885,
5656,
3976,
106,
286,
12087,
15733,
16559,
3249,
9471,
1983,
12281,
13369,
13150,
10799,
6608,
4394,
187,
15378,
4749,
3956,
6844,
9390,
14736,
7788,
17563,
17323,
10772,
10869,
8152,
16870,
8795,
5008,
6330,
7366,
18087,
2180,
5111,
5163,
12081,
34,
9797,
5755,
17260,
2367,
11304,
16116,
19974,
10,
3716,
9903,
3899,
7083,
16562,
14891,
9681,
12443,
5167,
1088,
1909,
13774,
3771,
7720,
1076,
10604,
2234,
3776,
10666,
6924,
11866,
9892,
19463,
1046,
10249,
373,
18594,
9147,
2141,
4721,
13856,
11018,
16865,
4428,
14114,
4231,
19646,
2891,
15527,
11064,
452,
3360,
3521,
1507,
4140,
2296,
6926,
5237,
7436,
1829,
1393,
6090,
2180,
14542,
13202,
7896,
6026,
13145,
4370,
14823,
3502,
9819,
10572,
8616,
7773,
5917,
4559,
3849,
14432,
9736,
3821,
15904,
10920,
10880,
19802,
14392,
8487,
240,
1175,
13858,
8012,
503,
18659,
6887,
6194,
9245,
15999,
3330,
15520,
2253,
7939,
16182,
18737,
14369,
5,
12315,
8229,
9160,
330,
17017,
12527,
10418,
4572,
5638,
15291,
14778,
7241,
10244,
12337,
16757,
9340,
15358,
1879,
11542,
19307,
4159,
5293,
11754,
19490,
2415,
8905,
19055,
8844,
12923,
15136,
5441,
16465,
17260,
17805,
6388,
11376,
16101,
7304,
4215,
14096,
18863,
8787,
12841,
15720,
14987,
16573,
10484,
14390,
16967,
17361,
5143,
3205,
13707,
16432,
12535,
18851,
2648,
2602,
11203,
7606,
15482,
7162,
1624,
10515,
14427,
3574,
6144,
2035,
1540,
19812,
5413,
19331,
6578,
6583,
1311,
5687,
8925,
6090,
9495,
14305,
3339,
16183,
4992,
13748,
11644,
19190,
14847,
18220,
10053,
13011,
8691,
13248,
19944,
11877,
13031,
7938,
9566,
12556,
19093,
10446,
14083,
17696,
12900,
4577,
2868,
15276,
1950,
3405,
8679,
6039,
15963,
13707,
646,
13429,
4477,
5397,
5085,
18167,
18929,
5507,
1764,
9963,
12840,
11289,
4749,
8921,
18377,
12856,
2850,
5070,
16392,
6954,
19401,
9645,
5700,
19603,
19880,
2183,
937,
13113,
13032,
614,
8271,
6872,
12568,
19444,
5457,
15999,
17236,
10771,
18153,
19265,
19359,
15824,
10755,
7630,
15007,
1359,
13987,
13550,
10443,
12159,
5076,
19363,
13903,
3873,
2679,
6482,
18594,
18965,
11530,
6733,
9974,
356,
2612,
2888,
878,
17752,
7883,
4982,
11195,
15812,
7095,
872,
9720,
15633,
13792,
10774,
6015,
14828,
19366,
3345,
1386,
14530,
579,
5730,
17411,
15170,
14628,
3799,
15068,
10854,
1156,
11034,
7283,
15833,
15366,
1857,
4355,
10049,
2192,
5982,
2200,
5910,
19233,
18920,
5013,
3640,
10938,
19602,
9209,
819,
14133,
19813,
7029,
403,
12191,
19162,
9244,
4148,
12411,
19136,
502,
13186,
3144,
2059,
2490,
10082,
19356,
10387,
10714,
19340,
16465,
12640,
1084,
7401,
12864,
10310,
7058,
16109,
9009,
19048,
18185,
9421,
18611,
3099,
2561,
3023,
12916,
19279,
19141,
1550,
4695,
12287,
9088,
13800,
3014,
4072,
19849,
19393,
17120,
13798,
6704,
14903,
4500,
2932,
9431,
17203,
11209,
14979,
14783,
425,
6955,
9853,
7908,
8941,
4084,
2826,
12681,
18145,
7276,
4407,
11764,
6413,
4433,
1735,
1566,
5347,
12167,
8593,
8151,
11998,
17846,
11004,
12396,
7475,
9102,
15155,
5668,
18954,
2269,
5372,
2251,
16112,
18151,
6987,
11795,
19779,
3834,
10719,
15385,
1151,
7983,
17864,
17814,
9162,
1199,
11277,
8080,
6462,
14918,
3858,
19835,
682,
2434,
985,
14225,
10461,
782,
2123,
13101,
16645,
9896,
887,
14095,
11005,
11190,
2322,
19162,
18900,
4744,
5632,
5397,
3993,
8322,
14814,
5071,
10497,
19255,
7938,
4261,
9072,
9161,
17370,
12949,
8505,
2069,
567,
8005,
15565,
15476,
15646,
2759,
16514,
10288,
5959,
1235,
6468,
7059,
4089,
12420,
16012,
10168,
11318,
5342,
12754,
16504,
15266,
19306,
17155,
815,
4899,
9373,
10139,
2896,
12511,
9662,
19974,
10921,
11394,
7706,
14846,
16557,
9772,
4017,
8058,
5674,
15882,
1274,
1789,
10607,
17585,
6315,
2503,
8039,
14138,
7829,
12710,
19197,
17496,
13008,
11006,
7689,
14491,
5380,
1429,
18783,
10140,
17744,
4782,
5625,
7895,
19089,
16214,
19062,
17901,
8529,
18251,
11546,
5805,
11350,
13441,
18995,
6789,
11183,
7001,
6623,
6769,
8186,
2735,
5683,
18030,
14315,
11902,
19645,
4314,
620,
18190,
4537,
13685,
15176,
19095,
2862,
18826,
14139,
14781,
5652,
10979,
19814,
19938,
143,
14158,
5429,
18235,
19335,
15960,
12584,
2259,
13506,
3802,
11727,
6314,
18894,
3622,
11204,
10008,
14005,
7699,
8744,
2047,
6713,
7881,
8735,
18884,
10319,
17855,
16670,
17176,
5549,
700,
9955,
15914,
8009,
8309,
16152,
11471,
14970,
14882,
11044,
2726,
5954,
17448,
11171,
19436,
2911,
9820,
14895,
4253,
1652,
11909,
14708,
6268,
12263,
6107,
190,
19349,
2826,
16152,
9213,
7887,
7468,
9740,
7169,
4061,
5814,
15776,
5040,
18821,
19583,
15017,
12245,
18500,
9258,
19680,
6336,
19080,
4455,
11786,
7447,
17629,
3639,
13774,
12385,
3866,
15477,
4324,
13438,
18081,
7971,
9140,
7939,
16671,
16955,
11004,
19239,
19587,
16572,
2242,
15546,
14021,
3887,
12165,
17127,
10947,
15918,
5883,
9652,
17000,
9263,
11979,
16871,
3046,
15060,
3524,
12666,
949,
11723,
19283,
15769,
10359,
12705,
5953,
16592,
6832,
13205,
17156,
16581,
11995,
4513,
11918,
452,
12951,
1962,
15667,
5107,
6314,
12070,
19228,
9761,
8106,
19200,
9134,
17575,
17164,
14392,
9462,
13010,
12060,
10638,
14127,
3563,
16610,
17576,
17031,
1511,
5320,
4647,
8096,
14989,
8617,
19549,
11528,
9505,
18354,
646,
19675,
14548,
18036,
19618,
17859,
7867,
9782,
15461,
1204,
19794,
7778,
3401,
10330,
17553,
2021,
9445,
14416,
16251,
11999,
14308,
7534,
18895,
14570,
16022,
19672,
11225,
17855,
3979,
3966,
9036,
2951,
19694,
5734,
15251,
6277,
13187,
5121,
9752,
13727,
5689,
17982,
5802,
8567,
2906,
17152,
14511,
1559,
18060,
1634,
3751,
6191,
5857,
1774,
17,
1893,
9235,
7624,
11382,
10145,
2118,
7123,
4998,
4054,
9008,
9671,
955,
10702,
9448,
11346,
2731,
6162,
2878,
7103,
19504,
4176,
18463,
1990,
3968,
7136,
11764,
13570,
17567,
3883,
16381,
8692,
1660,
19692,
15312,
11646,
16211,
77,
218,
17217,
11848,
8569,
1100,
14968,
19107,
19751,
45,
17186,
18372,
9905,
14428,
12605,
8030,
1463,
11174,
15371,
14778,
11486,
3325,
6980,
17500,
16906,
8446,
10737,
1854,
9543,
19035,
6884,
3459,
7803,
12210,
2401,
18335,
600,
13811,
3393,
8272,
1811,
15953,
15808,
9830,
11826,
17365,
4067,
6675,
10900,
9109,
10702,
8143,
1062,
2257,
9658,
9858,
19084,
14438,
15879,
8924,
8294,
964,
8873,
10146,
10449,
19735,
17702,
11391,
15474,
11252,
2603,
16584,
17643,
18044,
8059,
8460,
10624,
9597,
15885,
14948,
13071,
6279,
15960,
6506,
11192,
2329,
18375,
18079,
8683,
7952,
11699,
2930,
16645,
4859,
10680,
68,
11742,
12164,
1489,
1946,
3462,
3691,
18204,
1082,
7238,
8774,
6603,
19432,
7210,
6160,
7629,
5951,
17199,
707,
8034,
17061,
13386,
3267,
17535,
3855,
15870,
12527,
18494,
15669,
4652,
9030,
6544,
10793,
7925,
19807,
14486,
15760,
8805,
19462,
18945,
6522,
6478,
13297,
16333,
1692,
845,
13694,
815,
11486,
7960,
446,
5946,
3006,
4856,
1210,
6751,
4685,
1546,
18780,
6342,
3764,
12489,
9236,
9645,
15079,
10211,
10558,
19567,
5398,
10308,
7037,
13867,
11914,
12049,
9294,
11785,
18024,
11099,
19717,
13564,
19741,
12772,
7093,
4479,
445,
8983,
6190,
3302,
19177,
17882,
14787,
7961,
10479,
5440,
7747,
18853,
15176,
14329,
3399,
18075,
17508,
2831,
6686,
17754,
11824,
19326,
1970,
16723,
17658,
18740,
246,
17183,
6546,
19826,
3489,
5215,
5117,
15139,
17909,
15127,
1705,
15190,
103,
13794,
4241,
11903,
5571,
14830,
13761,
4921,
4823,
18433,
14064,
17145,
12329,
15050,
4646,
13578,
13312,
16008,
15065,
9142,
2859,
15936,
14561,
10340,
5032,
13591,
12932,
6717,
18877,
16798,
8849,
14754,
17103,
4839,
18205,
19287,
4635,
14739,
2930,
6983,
15780,
81,
3408,
15383,
8597,
7713,
1471,
412,
3158,
16279,
422,
9024,
2237,
12010,
19599,
15216,
16291,
14687,
2014,
11854,
18664,
1372,
5329,
3536,
3203,
17520,
15307,
17787,
19590,
14652,
2093,
16126,
8580,
14901,
14519,
6082,
1929,
7790,
9132,
19127,
16191,
12217,
1627,
5827,
16142,
3684,
30,
14699,
1235,
6792,
12015,
2694,
11850,
11000,
6006,
18210,
4222,
5281,
15418,
384,
9609,
15643,
9130,
11825,
14653,
14460,
18400,
17838,
3334,
17304,
10089,
7627,
7774,
19489,
550,
2619,
8924,
4082,
6556,
3480,
16468,
8718,
19031,
16235,
5250,
16874,
10461,
641,
8005,
14903,
13664,
3228,
1723,
9999,
14449,
6690,
19353,
3611,
5382,
16681,
19438,
16890,
16097,
6851,
6584,
11609,
2367,
6066,
9600,
6308,
6597,
3057,
19459,
18531,
1631,
18645,
8250,
8190,
18880,
18350,
7054,
12050,
17152,
4204,
8274,
1201,
11458,
17351,
7421,
470,
7530,
17065,
13938,
1480,
19510,
7110,
1821,
16168,
404,
17595,
17831,
13141,
16387,
14900,
15831,
14720,
16653,
16515,
6342,
8008,
4235,
7555,
15905,
9708,
19222,
9593,
12676,
234,
14360,
10122,
18400,
2837,
7157,
6053,
180,
4865,
15224,
10299,
5015,
6879,
2614,
9149,
4799,
14517,
4948,
15562,
8745,
5090,
16692,
18414,
9108,
16418,
10718,
14748,
7412,
3927,
16695,
3004,
17359,
9549,
7828,
2701,
13630,
9276,
10620,
2190,
8950,
19836,
9303,
11056,
4248,
17096,
2368,
19526,
9157,
12406,
10777,
1143,
5357,
17626,
15420,
9017,
1104,
8842,
10236,
16168,
13322,
15179,
11788,
1285,
5421,
107,
5188,
10692,
18500,
15634,
12029,
1584,
3192,
18995,
5202,
4458,
11463,
18950,
9047,
7553,
14034,
15639,
14916,
16757,
2376,
10238,
15178,
16786,
13089,
8316,
3410,
13113,
6567,
11177,
1304,
723,
1053,
5842,
18976,
12577,
1726,
4941,
12669,
14316,
6394,
15380,
5555,
14179,
16636,
15307,
13077,
1093,
1234,
17888,
3626,
18506,
943,
5877,
7937,
13113,
2899,
14050,
872,
17237,
17444,
9161,
4596,
15727,
18439,
3416,
4084,
2508,
13339,
13334,
3476,
14652,
5778,
4985,
12683,
814,
9506,
10829,
1519,
4072,
16721,
19284,
2566,
12396,
9151,
19696,
10753,
52,
15366,
11966,
17877,
3001,
8436,
3652,
649,
12515,
2676,
14828,
5771,
13592,
9722,
598,
7647,
15769,
19649,
12895,
2030,
12644,
8820,
1332,
17149,
1678,
6322,
8224,
6978,
13754,
19969,
9041,
12799,
4382,
4124,
7977,
8508,
13799,
2334,
17915,
8531,
3215,
618,
3069,
841,
13184,
5104,
4860,
16939,
12974,
3654,
10985,
19372,
11270,
13309,
9830,
8876,
14107,
17571,
19866,
14638,
18800,
13854,
2659,
6228,
5269,
1239,
17956,
864,
19712,
14007,
18525,
4090,
14195,
12779,
661,
14854,
2607,
566,
12390,
17425,
14317,
896,
13801,
8424,
14877,
242,
4147,
2903,
7972,
3992,
15983,
13126,
1722,
7881,
2205,
6944,
3753,
9310,
12448,
18744,
446,
8959,
6544,
15892,
9820,
13037,
10338,
1838,
6198,
17849,
12537,
10398,
8218,
10864,
2825,
17457,
5520,
18611,
11792,
19380,
8641,
11899,
2020,
6545,
2420,
10612,
5604,
15583,
12262,
13547,
12612,
11203,
11714,
4244,
6509,
14169,
17602,
14154,
20,
19766,
1493,
733,
15896,
2909,
2706,
8647,
7624,
4714,
13336,
476,
11334,
7516,
1058,
6569,
18615,
19710,
15106,
11003,
5875,
12506,
8008,
16915,
10415,
3726,
13264,
11219,
15481,
7145,
1553,
15465,
2159,
11549,
8544,
3054,
8118,
1425,
15789,
2756,
19066,
6691,
11868,
5744,
8528,
13336,
5433,
10723,
12259,
10311,
2552,
12049,
112,
16058,
9510,
17825,
552,
11114,
16754,
17673,
13121,
9475,
3173,
17747,
19480,
15686,
14781,
12244,
2040,
19282,
6445,
18602,
6323,
16616,
14243,
19396,
6245,
19483,
6891,
4012,
17851,
2784,
13579,
19051,
5959,
5151,
19897,
3249,
7189,
14340,
14047,
16722,
2744,
2948,
4399,
3729,
7136,
14946,
13263,
15321,
11201,
19597,
19008,
18501,
10964,
14692,
10787,
5582,
7831,
13128,
11478,
7876,
1743,
4002,
6166,
9255,
3038,
18809,
11383,
12156,
4223,
16152,
7745,
5039,
1427,
8191,
1196,
17573,
9176,
10332,
1697,
9140,
8112,
8030,
17565,
19627,
7638,
16072,
19486,
940,
9192,
211,
10535,
7481,
16342,
13962,
2749,
7378,
287,
7249,
15783,
17955,
13945,
3303,
4638,
15966,
6710,
10121,
12944,
10507,
7131,
10,
16993,
1361,
2472,
17471,
1400,
17843,
16063,
19170,
19285,
9614,
16017,
9327,
6320,
16772,
1842,
10607,
11325,
18774,
5940,
5887,
14620,
16228,
7711,
4664,
7866,
2019,
11068,
7688,
10959,
6323,
7787,
19623,
13883,
4628,
662,
3912,
2308,
2257,
14486,
13531,
12630,
16945,
1963,
1284,
19262,
13808,
751,
163,
19639,
12888,
19226,
19013,
2463,
15974,
8378,
15656,
4314,
10852,
18866,
12218,
10014,
729,
13266,
5437,
10049,
12000,
5572,
2751,
6167,
10864,
9579,
11445,
18897,
3399,
18881,
5087,
14702,
15763,
1054,
2643,
15584,
15838,
4855,
15293,
6202,
10267,
19747,
19741,
14197,
4620,
18797,
14899,
13838,
8465,
2910,
10318,
2927,
130,
10571,
7414,
14440,
17533,
6343,
5796,
8525,
13294,
386,
5239,
1022,
7874,
1601,
11845,
17308,
12260,
6749,
17604,
1576,
14956,
13593,
1670,
17211,
6807,
12189,
4322,
16579,
18359,
14792,
17551,
13201,
18808,
14620,
12531,
1324,
17472,
16613,
14960,
6873,
17366,
18046,
16263,
18686,
3750,
1897,
4106,
19893,
3565,
17907,
14772,
17811,
8211,
17242,
5038,
18340,
13161,
2222,
8606,
13570,
11890,
4186,
15233,
3634,
4652,
9704,
9947,
3991,
8619,
8905,
739,
11005,
8191,
10544,
6180,
12221,
6196,
2247,
1053,
10312,
5589,
808,
13131,
11058,
12125,
3322,
5646,
6068,
8270,
1838,
13136,
2584,
145,
17435,
1306,
6111,
8994,
16565,
14648,
9118,
14305,
8914,
17561,
7615,
8036,
6043,
11711,
18392,
13671,
15484,
15253,
18236,
6946,
8433,
14460,
11356,
3446,
3951,
1324,
18434,
6625,
12324,
14936,
11189,
622,
4648,
1246,
14818,
10751,
8344,
14680,
15995,
5394,
3919,
3064,
8815,
18414,
2841,
1595,
11763,
18126,
7929,
6218,
16512,
12677,
18281,
18508,
8867,
16754,
10836,
3817,
14446,
9358,
15882,
7879,
10023,
19118,
5428,
5176,
10004,
13963,
13482,
13462,
9952,
10699,
2067,
4720,
14631,
7074,
6364,
12118,
12057,
9062,
11364,
6706,
3439,
4605,
4695,
2189,
18166,
18626,
11834,
1513,
18989,
13599,
5138,
10313,
7452,
13776,
2602,
8080,
19334,
3678,
15921,
1135,
12090,
359,
16762,
13375,
10646,
12824,
16580,
19280,
15269,
19144,
18437,
7873,
10750,
19433,
9735,
10849,
9538,
4106,
10433,
15223,
19795,
9208,
9658,
16683,
4313,
2680,
2520,
2731,
3660,
14037,
18650,
4471,
16379,
9980,
10433,
5847,
13636,
16357,
13238,
2230,
10807,
18908,
11303,
13273,
20,
12582,
10803,
7109,
14718,
7777,
18792,
8644,
10965,
18139,
2573,
8026,
19980,
11225,
14873,
5487,
2000,
11144,
15579,
19270,
13717,
4188,
4529,
18025,
6816,
16829,
18046,
13280,
15372,
10925,
6159,
1454,
14376,
1308,
399,
10303,
10473,
6788,
6609,
773,
2429,
11948,
13454,
11372,
5419,
13355,
1555,
4657,
15781,
6762,
2640,
8401,
1560,
16274,
5910,
19677,
13786,
2595,
14236,
17578,
13449,
16193,
12979,
16979,
177,
6010,
8821,
15945,
3869,
3084,
10344,
13187,
12831,
13874,
11075,
16429,
8895,
8771,
11179,
18491,
1120,
19060,
13460,
5058,
4145,
8368,
5349,
174,
18097,
18532,
4328,
19534,
3282,
9196,
4898,
16888,
8847,
8407,
9912,
3605,
10223,
16962,
16306,
4496,
15339,
15885,
4051,
12996,
9946,
16304,
13989,
16137,
7739,
6404,
16827,
4314,
10570,
18591,
10533,
7859,
6297,
15782,
7532,
17088,
15579,
11289,
8056,
4764,
15035,
7661,
18161,
8693,
11434,
19574,
3013,
2505,
3471,
2370,
487,
11149,
5078,
6837,
10032,
829,
11555,
9096,
3489,
16440,
2134,
8584,
12531,
6163,
18404,
12726,
18417,
2126,
15592,
8236,
18552,
8279,
9275,
3466,
8087,
4606,
3036,
19461,
11683,
17124,
8834,
7737,
4020,
16944,
10104,
17975,
17331,
2187,
2538,
12807,
14012,
15166,
6054,
7464,
18588,
2617,
2285,
16531,
8575,
1502,
19407,
2983,
3621,
8712,
1722,
7072,
13331,
8260,
17192,
5348,
7961,
19698,
968,
9173,
97,
14821,
4889,
18229,
17845,
19533,
13479,
2987,
10763,
11507,
7592,
6886,
12464,
8042,
11627,
19018,
15922,
3143,
16481,
15402,
813,
15222,
5520,
18783,
19397,
11371,
11425,
17626,
19784,
1545,
19408,
19866,
13847,
7212,
876,
3118,
7054,
18865,
5679,
15367,
9750,
6342,
5594,
4459,
3966,
14113,
142,
6991,
19184,
4672,
4358,
1444,
3953,
63,
16859,
16998,
12860,
13639,
9294,
2710,
7032,
16666,
16918,
2244,
18932,
15739,
333,
3123,
4101,
18982,
19084,
18629,
13951,
11517,
18219,
9212,
10071,
16830,
10942,
19945,
7978,
7188,
775,
5980,
1629,
1948,
18545,
245,
12134,
16245,
1912,
18952,
967,
1516,
6672,
18957,
9366,
328,
13355,
11469,
18719,
13257,
11233,
7372,
11333,
13712,
8770,
2435,
19735,
6530,
16976,
2925,
2562,
3058,
16626,
18523,
13171,
5064,
10457,
9423,
13876,
2666,
16771,
8283,
10954,
8880,
7235,
18217,
694,
5875,
8871,
2074,
18279,
13337,
12388,
8528,
7078,
15174,
4804,
9047,
12466,
11840,
16783,
6539,
6912,
2078,
14991,
14309,
5371,
16358,
3654,
6490,
6125,
277,
1521,
14734,
7104,
6905,
551,
15881,
6618,
3033,
12104,
18520,
7514,
14457,
6637,
4639,
17247,
2986,
6731,
6805,
10239,
9762,
7732,
15727,
9804,
5646,
6920,
2002,
511,
3392,
8091,
17033,
4256,
19506,
18645,
1274,
15105,
10145,
8415,
6351,
12929,
15909,
8838,
9227,
9416,
19395,
3926,
8431,
5682,
14682,
1856,
5796,
7575,
17750,
16518,
12491,
3770,
14349,
7071,
11591,
7172,
5856,
7551,
14304,
15116,
9324,
5357,
4367,
18269,
9442,
4910,
14638,
1655,
17405,
9641,
11406,
10674,
6532,
7015,
5625,
18137,
2087,
9499,
10782,
3961,
3015,
13554,
15743,
1955,
13893,
12763,
18354,
7816,
32,
16492,
11528,
5406,
4006,
16076,
4644,
9107,
13416,
4730,
16203,
4439,
4005,
456,
16573,
14533,
11862,
16421,
16732,
19098,
9749,
19366,
4013,
6707,
12984,
13312,
12992,
9949,
564,
15180,
12197,
16890,
8627,
6545,
5318,
15505,
8507,
17952,
13334,
9077,
11909,
2331,
11459,
15326,
16361,
3359,
11314,
379,
5561,
1151,
18318,
18313,
1354,
15354,
1380,
11412,
19332,
5295,
11012,
12983,
17593,
14526,
6784,
4999,
1185,
13008,
16598,
10706,
17763,
10933,
18103,
3819,
5825,
667,
18823,
9569,
13057,
9189,
3105,
1691,
1684,
15206,
19873,
7222,
18164,
18307,
12965,
4702,
15964,
4157,
5721,
7498,
12386,
19369,
17350,
988,
9128,
2284,
4130,
18140,
14124,
10249,
7901,
7487,
12955,
19402,
2797,
1417,
9776,
19675,
19440,
880,
17048,
6880,
6150,
3554,
15204,
5467,
8622,
15872,
5702,
916,
2686,
1715,
854,
9096,
5612,
2065,
18941,
9011,
17474,
94,
15310,
13893,
1108,
9040,
4878,
6850,
19968,
3,
16935,
612,
16928,
2913,
12793,
12520,
11657,
867,
15778,
18211,
18424,
13210,
12872,
8883,
6495,
13543,
14079,
15133,
3901,
1124,
7152,
4312,
342,
16000,
17010,
19077,
16762,
6587,
8375,
8458,
3455,
18670,
6166,
6316,
3511,
14109,
18583,
2337,
17250,
19675,
16677,
16804,
15640,
16904,
11076,
13290,
3450,
18216,
7653,
10773,
2275,
8888,
19699,
16427,
11823,
9611,
3116,
19381,
2831,
1585,
5114,
18073,
5261,
7588,
6818,
9019,
19757,
18195,
8010,
12647,
233,
16182,
4173,
10747,
11819,
9075,
10659,
739,
12421,
9205,
14505,
12495,
8192,
4291,
19677,
4013,
1697,
16106,
9204,
10370,
6366,
4400,
8224,
19488,
13579,
10904,
1783,
19147,
4659,
18644,
7178,
17857,
9845,
11704,
3494,
11154,
15542,
18899,
8754,
16315,
4013,
2496,
7827,
15784,
9904,
3363,
15756,
15757,
10556,
8395,
18799,
4207,
2600,
18930,
7617,
9417,
12611,
16522,
17443,
1181,
17262,
4877,
18285,
5451,
12451,
18570,
3495,
7688,
15693,
16161,
8296,
3736,
6704,
9368,
5189,
1358,
12484,
8103,
9954,
17016,
5339,
7955,
5209,
9648,
19966,
12322,
4490,
801,
13932,
6302,
18270,
8879,
10282,
5435,
6250,
16892,
17943,
12049,
5757,
951,
19409,
6152,
3279,
540,
17301,
14942,
6702,
13468,
2017,
6501,
2918,
5568,
14636,
10503,
15826,
1062,
8069,
2938,
688,
17679,
11924,
15203,
11038,
615,
12081,
1191,
9435,
15099,
14288,
9091,
11808,
5318,
6034,
7859,
18410,
15627,
8210,
17720,
6439,
18830,
19469,
17466,
16579,
19651,
9636,
15216,
9405,
17283,
10010,
4113,
6634,
3889,
7657,
19503,
16102,
13043,
10112,
14791,
17811,
7390,
5730,
10783,
8305,
14334,
10709,
12157,
15439,
18878,
10349,
9410,
14383,
1228,
16949,
15531,
12058,
17429,
7543,
19806,
7016,
1080,
2825,
10980,
10561,
8828,
13223,
3038,
7119,
7221,
1449,
14046,
8760,
5810,
14678,
10591,
18271,
19261,
19788,
18033,
14159,
822,
2701,
7849,
11700,
18456,
17167,
10615,
15736,
1147,
6818,
16684,
8763,
5777,
17651,
13277,
16623,
2274,
8627,
8434,
19365,
2738,
2577,
14778,
14437,
10097,
15659,
7838,
1557,
10933,
19124,
10351,
11077,
19643,
1796,
4951,
3831,
19398,
3626,
10579,
17943,
8450,
694,
13308,
12440,
8615,
14165,
13613,
17115,
16195,
15752,
5537,
2922,
8389,
9776,
10853,
12176,
6471,
7101,
6585,
1771,
10248,
13437,
18596,
7598,
18909,
10707,
15089,
11614,
11643,
3148,
573,
5461,
5072,
8588,
19506,
6787,
12433,
7976,
643,
12617,
8877,
12403,
2147,
15230,
14550,
4407,
5089,
2274,
7381,
2619,
10098,
11510,
8739,
3728,
19332,
10159,
7920,
10404,
7893,
5855,
7893,
17253,
5681,
15164,
14238,
9638,
4094,
2922,
10347,
13774,
2590,
8491,
14978,
5494,
8201,
17270,
5581,
3521,
307,
16070,
16432,
8840,
5763,
11858,
15141,
10507,
15803,
5745,
12298,
9278,
3172,
1774,
14417,
16111,
897,
6888,
15482,
15763,
19043,
12437,
12163,
6518,
9584,
4350,
17701,
13156,
3184,
14975,
12090,
18140,
8616,
15980,
4932,
15949,
7579,
10026,
2510,
6982,
5452,
12197,
14020,
7035,
3103,
1855,
9577,
16364,
19342,
1854,
3309,
1618,
17943,
13400,
7055,
18408,
13658,
2093,
10503,
4046,
15223,
10853,
5612,
19580,
12723,
10294,
9421,
17085,
18135,
10222,
6233,
2152,
10545,
16721,
19019,
16003,
777,
14731,
16616
]

fun p f profile =
    if profile then
        P.profile P.ProfileTime f () handle e => Ponyo.Format.println [exnName e, exnMessage e]
    else
        f ()

fun t () =
    let
        val d = ref (D.newWithSize (7500))
        val i = ref 0
    in
        p (fn () => (List.map (fn (k) => (d := D.insert (!d) k (!i); i := !i + 1)) a; ())) false;
        p (fn () => (app (fn (k) => (D.get (!d) k; ())) (List.rev a); ())) true;
        ()
    end

fun m () =
    let
        val d = ref M.new
        val i = ref 0
    in
        p (fn () => (List.map (fn (k) => (d := M.insert (!d) k (!i); i := !i + 1)) a; ())) false;
        p (fn () => (app (fn (k) => (M.get (!d) k; ())) (List.rev a); ())) true;
        ()
    end

fun main () =
    let in
        p t false;
        p m false
    end