if ((typeof(rwp$0Rwp$1)) == "undefined") {rwp$0Rwp$1 = {};
};
(function(){rwp$0Rwp$1.eq_kind$1149 = function(v$1151,v$1152){switch (v$1151) { case 0: {switch (v$1152) { case 0: {return true;
 break; }default: {return false;
} };
 break; }case 1: {switch (v$1152) { case 1: {return true;
 break; }default: {return false;
} };
 break; } };
};
rwp$0Rwp$1.empty$62 = function(f$65){return [f$65,null];
};
var en$SAME$68 = new String("SAME");
var exn$SAME$68 = [en$SAME$68];
rwp$0Rwp$1.put$104 = function(t$109){var v$118 = t$109[1];
var v$119 = t$109[0];
return function(v$112){try {var t$2366 = v$119;
var t$2365;
var fix$2364 = {};
fix$2364.$insert = function(v$2260,v$2278){if (v$2260 == null) {return [v$2278,null];
} else {var v$2261 = v$2260;
var v$2262 = v$2261[0];
var v$2263 = v$2261[1];
if ((v$119(v$2278)) < (v$119(v$2262))) {return [v$2278,v$2260];
} else {if ((v$119(v$2262)) < (v$119(v$2278))) {return [v$2262,fix$2364.$insert(v$2263,v$2278)];
} else {throw exn$SAME$68;
};
};
};
};
var insert$2258 = fix$2364.$insert;
t$2365 = (insert$2258(v$118,v$112));
return [t$2366,t$2365];
} catch(v$2363) {return (function(SAME$117){var t$2367 = SAME$117;
if (t$2367[0] == en$SAME$68) {return t$109;
} else {throw SAME$117;
};
})(v$2363);
};
};
};
rwp$0Rwp$1.get$120 = function(v$2279,v$131){if (v$131 == null) {return [1];
} else {var v$134 = v$131;
return [0,[v$134[0],[v$2279,v$134[1]]]];
};
};
var c$138 = [0];
var c$151 = [1];
var h$156 = [[function(v$159){return v$159[1];
},null]];
var fix$2368 = {};
fix$2368.$eval = function(v$211){lab$eval: while (true) {var v$216;
var v$1305;
var v$1306 = h$156[0];
var v$1307 = v$1306[1];
if (v$1307 == null) {v$1305 = [1];
} else {var v$1308 = v$1307;
v$1305 = [0,[v$1308[0],[v$1306[0],v$1308[1]]]];
};
switch (v$1305[0]) { case 0: {var v$1309 = v$1305[1];
v$216 = [[0,v$1309[0]],(h$156[0] = v$1309[1],0)][0];
 break; }default: {v$216 = [1];
} };
switch (v$216[0]) { case 1: {return 0;
 break; }default: {var v$223 = v$216[1];
v$223[0](0);
var t$2369 = 0;
var v$211 = t$2369;
continue lab$eval;
} };
};
};
var eval$209 = fix$2368.$eval;
var newValue$232 = function(v$1154,b$235){var v$240 = b$235[0];
switch (v$240[0]) { case 0: {var v$251 = v$240[1];
return function(v$244){var t$2370;
var v$1914 = v$251[0];
t$2370 = (v$1154([v$244,v$1914]));
if (t$2370) {return 0;
} else {(v$251[0] = v$244,0);
var p$1321 = [function(v$1329){var v$1330 = b$235[0];
switch (v$1330[0]) { case 0: {var v$1331 = v$1330[1];
var v$1332 = v$1331[0];
var fix$2371 = {};
fix$2371.$app = function(v$1334){lab$app: while (true) {if (v$1334 == null) {return 0;
} else {var v$1335 = v$1334;
var v$1336 = v$1335[0];
var v$1337 = v$1335[1];
var v$1339 = v$1336[1];
v$1339(v$1332);
var t$2372 = v$1337;
var v$1334 = t$2372;
continue lab$app;
};
};
};
var app$1333 = fix$2371.$app;
return app$1333(basis$0List$1.rev$678(b$235[1][0]));
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"pack.comp.NONE"];
} };
},b$235[2]];
var t$2379 = h$156;
var t$2373;
var t$1323 = h$156[0];
var v$1324 = t$1323[1];
var v$1325 = t$1323[0];
try {var t$2377 = v$1325;
var t$2376;
var fix$2375 = {};
fix$2375.$insert = function(v$2268,v$2280){if (v$2268 == null) {return [v$2280,null];
} else {var v$2269 = v$2268;
var v$2270 = v$2269[0];
var v$2271 = v$2269[1];
if ((v$1325(v$2280)) < (v$1325(v$2270))) {return [v$2280,v$2268];
} else {if ((v$1325(v$2270)) < (v$1325(v$2280))) {return [v$2270,fix$2375.$insert(v$2271,v$2280)];
} else {throw exn$SAME$68;
};
};
};
};
var insert$2266 = fix$2375.$insert;
t$2376 = (insert$2266(v$1324,p$1321));
t$2373 = [t$2377,t$2376];
} catch(v$2374) {t$2373 = ((function(SAME$1327){var t$2378 = SAME$1327;
if (t$2378[0] == en$SAME$68) {return t$1323;
} else {throw SAME$1327;
};
})(v$2374));
};
return (t$2379[0] = t$2373,0);
};
};
 break; }default: {return function(v$254){var l$1341 = basis$0List$1.rev$678(b$235[1][0]);
var fix$2380 = {};
fix$2380.$app = function(v$1343){lab$app: while (true) {if (v$1343 == null) {return 0;
} else {var v$1344 = v$1343;
var v$1345 = v$1344[0];
var v$1346 = v$1344[1];
var v$2137 = v$1345[1];
v$2137(v$254);
var t$2381 = v$1346;
var v$1343 = t$2381;
continue lab$app;
};
};
};
var app$1342 = fix$2380.$app;
return app$1342(l$1341);
};
} };
};
rwp$0Rwp$1.current$267 = function(v$1155,b$270){var v$275 = b$270[0];
switch (v$275[0]) { case 0: {return v$275[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
};
rwp$0Rwp$1.addListener$282 = function(v$2281,v$289,v$2282){return function(f$288){return (v$289[0] = [[0,f$288],v$289[0]],0);
};
};
rwp$0Rwp$1.send$344 = function(v$1156,b$347,v$350){return (newValue$232(v$1156,b$347))(v$350);
};
var pairT$421 = function(v$1161,v$1162,v$425){var v$483 = v$425[0];
var v$484 = v$425[1];
var v$1921 = v$483[0];
var v$1922 = v$484[0];
switch (v$1921[0]) { case 0: {switch (v$1922[0]) { case 0: {var v$450 = v$1921[1];
var v$451 = v$1922[1];
var e$439;
var init$1389 = [0,[v$450[0],v$451[0]]];
var v$1390 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1392 = [null];
e$439 = [basis$0Option$1.map$65(function(v$1393){return [v$1393];
},init$1389),v$1392,v$1390];
var v$1397 = v$483[1];
(v$1397[0] = [[0,function(v$2138){return (newValue$232(function(v$2139){if (v$1162([v$2139[0][0],v$2139[1][0]])) {return v$1161([v$2139[0][1],v$2139[1][1]]);
} else {return false;
};
},e$439))([v$2138,v$451[0]]);
}],v$1397[0]],0);
var v$1402 = v$484[1];
(v$1402[0] = [[0,function(v$2140){return (newValue$232(function(v$2141){if (v$1162([v$2141[0][0],v$2141[1][0]])) {return v$1161([v$2141[0][1],v$2141[1][1]]);
} else {return false;
};
},e$439))([v$450[0],v$2140]);
}],v$1402[0]],0);
return e$439;
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"pairT.impossible"];
} };
 break; }default: {switch (v$1922[0]) { case 1: {var e1s$454 = [null];
var e2s$455 = [null];
var e$456;
var init$1408 = [1];
var v$1409 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1411 = [null];
e$456 = [basis$0Option$1.map$65(function(v$1412){return [v$1412];
},init$1408),v$1411,v$1409];
var v$1414 = v$483[1];
(v$1414[0] = [[0,function(v$2142){var v$2143;
if (e2s$455[0] == null) {v$2143 = [1];
} else {var v$2144 = e2s$455[0];
v$2143 = [[0,v$2144[0]],(e2s$455[0] = v$2144[1],0)][0];
};
switch (v$2143[0]) { case 1: {return (e1s$454[0] = (basis$0List$1.rev$678([v$2142,basis$0List$1.rev$678(e1s$454[0])])),0);
 break; }default: {var v$2145 = v$2143[1];
return (newValue$232(function(v$2146){if (v$1162([v$2146[0][0],v$2146[1][0]])) {return v$1161([v$2146[0][1],v$2146[1][1]]);
} else {return false;
};
},e$456))([v$2142,v$2145]);
} };
}],v$1414[0]],0);
var v$1426 = v$484[1];
(v$1426[0] = [[0,function(v$2147){var v$2148;
if (e1s$454[0] == null) {v$2148 = [1];
} else {var v$2149 = e1s$454[0];
v$2148 = [[0,v$2149[0]],(e1s$454[0] = v$2149[1],0)][0];
};
switch (v$2148[0]) { case 1: {return (e2s$455[0] = (basis$0List$1.rev$678([v$2147,basis$0List$1.rev$678(e2s$455[0])])),0);
 break; }default: {var v$2150 = v$2148[1];
return (newValue$232(function(v$2151){if (v$1162([v$2151[0][0],v$2151[1][0]])) {return v$1161([v$2151[0][1],v$2151[1][1]]);
} else {return false;
};
},e$456))([v$2150,v$2147]);
} };
}],v$1426[0]],0);
return e$456;
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"pairT.impossible"];
} };
} };
};
rwp$0Rwp$1.pair$485 = function(v$1169,v$1170){return function(v$2287){return pairT$421(v$1169,v$1170,v$2287);
};
};
rwp$0Rwp$1.tup3$486 = function(v$1171,v$1172,v$1173,v$491){var v$533 = v$491[0];
var v$534 = v$491[1];
var v$535 = v$491[2];
var v$1943 = v$533[0];
var v$1944 = v$534[0];
var v$1945 = v$535[0];
switch (v$1943[0]) { case 0: {switch (v$1944[0]) { case 0: {switch (v$1945[0]) { case 0: {var v$517 = v$1943[1];
var v$518 = v$1944[1];
var v$519 = v$1945[1];
var e$507;
var init$1439 = [0,[v$517[0],v$518[0],v$519[0]]];
var v$1440 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1442 = [null];
e$507 = [basis$0Option$1.map$65(function(v$1443){return [v$1443];
},init$1439),v$1442,v$1440];
var v$1448 = v$533[1];
(v$1448[0] = [[0,function(v$2152){return (newValue$232(function(v$2153){if (v$1173([v$2153[0][0],v$2153[1][0]])) {if (v$1172([v$2153[0][1],v$2153[1][1]])) {return v$1171([v$2153[0][2],v$2153[1][2]]);
} else {return false;
};
} else {return false;
};
},e$507))([v$2152,v$518[0],v$519[0]]);
}],v$1448[0]],0);
var v$1454 = v$534[1];
(v$1454[0] = [[0,function(v$2154){return (newValue$232(function(v$2155){if (v$1173([v$2155[0][0],v$2155[1][0]])) {if (v$1172([v$2155[0][1],v$2155[1][1]])) {return v$1171([v$2155[0][2],v$2155[1][2]]);
} else {return false;
};
} else {return false;
};
},e$507))([v$517[0],v$2154,v$519[0]]);
}],v$1454[0]],0);
var v$1460 = v$535[1];
(v$1460[0] = [[0,function(v$2156){return (newValue$232(function(v$2157){if (v$1173([v$2157[0][0],v$2157[1][0]])) {if (v$1172([v$2157[0][1],v$2157[1][1]])) {return v$1171([v$2157[0][2],v$2157[1][2]]);
} else {return false;
};
} else {return false;
};
},e$507))([v$517[0],v$518[0],v$2156]);
}],v$1460[0]],0);
return e$507;
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"tup3.impossible"];
} };
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"tup3.impossible"];
} };
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"tup3.impossible"];
} };
};
rwp$0Rwp$1.merge$536 = function(v$1178,v$540){var v$546 = v$540[0];
var v$547 = v$540[1];
var e$541;
var init$1469 = [1];
var v$1470 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1472 = [null];
e$541 = [basis$0Option$1.map$65(function(v$1473){return [v$1473];
},init$1469),v$1472,v$1470];
var v$1475 = v$546[1];
(v$1475[0] = [[0,newValue$232(v$1178,e$541)],v$1475[0]],0);
var v$1479 = v$547[1];
(v$1479[0] = [[0,newValue$232(v$1178,e$541)],v$1479[0]],0);
return e$541;
};
rwp$0Rwp$1.insertDOM_elem$548 = function(e$551,b$554){var v$559 = b$554[0];
switch (v$559[0]) { case 0: {var v$563 = v$559[1];
var v$555 = v$563[0];
(function(e,s){e.innerHTML = s;})(e$551,v$555);
var v$1485 = b$554[1];
return (v$1485[0] = [[0,function(s$2158){return (function(e,s){e.innerHTML = s;})(e$551,s$2158);
}],v$1485[0]],0);
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"insertDOM_elem impossible"];
} };
};
rwp$0Rwp$1.insertDOM$568 = function(id$571,b$574){var v$579 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$84,id$571);
switch (v$579[0]) { case 0: {var v$581 = v$579[1];
var v$2161 = b$574[0];
switch (v$2161[0]) { case 0: {var v$2162 = v$2161[1];
var v$2163 = v$2162[0];
(function(e,s){e.innerHTML = s;})(v$581,v$2163);
var v$2164 = b$574[1];
return (v$2164[0] = [[0,function(s$2165){return (function(e,s){e.innerHTML = s;})(v$581,s$2165);
}],v$2164[0]],0);
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"insertDOM_elem impossible"];
} };
 break; }default: {throw [basis$0Initial$1.en$Fail$50,("insertDOM: element with id=" + id$571) + " not in dom"];
} };
};
rwp$0Rwp$1.setStyle_elem$582 = function(e$585,v$589){var v$606 = v$589[0];
var v$607 = v$589[1];
var v$594 = v$607[0];
switch (v$594[0]) { case 0: {var v$601 = v$594[1];
var v$590 = v$601[0];
(function(fp,s,v){fp[s] = v;})((function(fp,s){return fp[s];})(e$585,"style"),v$606,v$590);
var v$1502 = v$607[1];
return (v$1502[0] = [[0,function(v$2166){return (function(fp,s,v){fp[s] = v;})((function(fp,s){return fp[s];})(e$585,"style"),v$606,v$2166);
}],v$1502[0]],0);
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"setStyle_elem impossible"];
} };
};
rwp$0Rwp$1.setAttr_elem$608 = function(e$611,v$615){var v$632 = v$615[0];
var v$633 = v$615[1];
var v$620 = v$633[0];
switch (v$620[0]) { case 0: {var v$627 = v$620[1];
var v$616 = v$627[0];
(function(e,a,b){return e.setAttribute(a,b);})(e$611,v$632,v$616);
var v$1513 = v$633[1];
return (v$1513[0] = [[0,function(v$2167){return (function(e,a,b){return e.setAttribute(a,b);})(e$611,v$632,v$2167);
}],v$1513[0]],0);
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"setAttr_elem impossible"];
} };
};
rwp$0Rwp$1.setStyle$634 = function(id$637,v$641){var v$649 = v$641[0];
var v$650 = v$641[1];
var v$646 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$84,id$637);
switch (v$646[0]) { case 0: {var v$648 = v$646[1];
return rwp$0Rwp$1.setStyle_elem$582(v$648,[v$649,v$650]);
 break; }default: {throw [basis$0Initial$1.en$Fail$50,("setStyle: element with id=" + id$637) + " not in dom"];
} };
};
rwp$0Rwp$1.setAttr$651 = function(id$654,v$658){var v$666 = v$658[0];
var v$667 = v$658[1];
var v$663 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$84,id$654);
switch (v$663[0]) { case 0: {var v$665 = v$663[1];
var v$2172 = v$667[0];
switch (v$2172[0]) { case 0: {var v$2173 = v$2172[1];
var v$2174 = v$2173[0];
(function(e,a,b){return e.setAttribute(a,b);})(v$665,v$666,v$2174);
var v$2175 = v$667[1];
return (v$2175[0] = [[0,function(v$2176){return (function(e,a,b){return e.setAttribute(a,b);})(v$665,v$666,v$2176);
}],v$2175[0]],0);
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"setAttr_elem impossible"];
} };
 break; }default: {throw [basis$0Initial$1.en$Fail$50,("setAttr: element with id=" + id$654) + " not in dom"];
} };
};
rwp$0Rwp$1.delay$668 = function(v$1179,ms$671,b$674){var b$$675;
var init$1534;
var v$680 = b$674[0];
switch (v$680[0]) { case 0: {var v$682 = v$680[1];
var v$676 = v$682[0];
init$1534 = [0,v$676];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"delay.impossible"];
} };
var v$1535 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1537 = [null];
b$$675 = [basis$0Option$1.map$65(function(v$1538){return [v$1538];
},init$1534),v$1537,v$1535];
var v$1540 = b$674[1];
(v$1540[0] = [[0,function(v$2177){(function(i,f){return setTimeout(f,i);})(ms$671,function(v$2178){(newValue$232(v$1179,b$$675))(v$2177);
return eval$209(0);
});
return 0;
}],v$1540[0]],0);
return b$$675;
};
rwp$0Rwp$1.calm$698 = function(v$1180,ms$701,b$704){var b$$705;
var init$1546;
var v$710 = b$704[0];
switch (v$710[0]) { case 0: {var v$712 = v$710[1];
var v$706 = v$712[0];
init$1546 = [0,v$706];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"calm.impossible"];
} };
var v$1547 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1549 = [null];
b$$705 = [basis$0Option$1.map$65(function(v$1550){return [v$1550];
},init$1546),v$1549,v$1547];
var c$717 = [0];
var v$1557 = b$704[1];
(v$1557[0] = [[0,function(v$2179){(c$717[0] = (SmlPrims.chk_ovf_i32(c$717[0] + 1)),0);
(function(i,f){return setTimeout(f,i);})(ms$701,function(v$2180){var t$2382;
(c$717[0] = (SmlPrims.chk_ovf_i32(c$717[0] - 1)),0);
t$2382 = (c$717[0] == 0);
if (t$2382) {(newValue$232(v$1180,b$$705))(v$2179);
return eval$209(0);
} else {return 0;
};
});
return 0;
}],v$1557[0]],0);
return b$$705;
};
rwp$0Rwp$1.textField_elem$745 = function(e$748){var b$749;
var init$1567 = [0,(function(e){return e.value;})(e$748)];
var v$1568 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1570 = [null];
b$749 = [basis$0Option$1.map$65(function(v$1571){return [v$1571];
},init$1567),v$1570,v$1568];
js$0Js$1.installEventHandler$193(e$748,2,function(v$1574){(newValue$232(function(v$1575){return v$1575[0] == v$1575[1];
},b$749))((function(e){return e.value;})(e$748));
eval$209(0);
return true;
});
return b$749;
};
rwp$0Rwp$1.textField$759 = function(id$762){var v$767 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$84,id$762);
switch (v$767[0]) { case 0: {var v$769 = v$767[1];
return rwp$0Rwp$1.textField_elem$745(v$769);
 break; }default: {throw [basis$0Initial$1.en$Fail$50,("textField: element with id=" + id$762) + " not in dom"];
} };
};
rwp$0Rwp$1.mouseOver_elem$770 = function(e$773){var b$774;
var init$1584 = [0,false];
var v$1585 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1587 = [null];
b$774 = [basis$0Option$1.map$65(function(v$1588){return [v$1588];
},init$1584),v$1587,v$1585];
js$0Js$1.installEventHandler$193(e$773,0,function(v$1590){(newValue$232(function(v$1591){return v$1591[0] == v$1591[1];
},b$774))(true);
eval$209(0);
return true;
});
js$0Js$1.installEventHandler$193(e$773,1,function(v$1593){(newValue$232(function(v$1594){return v$1594[0] == v$1594[1];
},b$774))(false);
eval$209(0);
return true;
});
return b$774;
};
rwp$0Rwp$1.mouseOver$789 = function(id$792){var v$797 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$84,id$792);
switch (v$797[0]) { case 0: {var v$799 = v$797[1];
return rwp$0Rwp$1.mouseOver_elem$770(v$799);
 break; }default: {throw [basis$0Initial$1.en$Fail$50,("mouseOver: element with id=" + id$792) + " not in dom"];
} };
};
rwp$0Rwp$1.mouse_elem$800 = function(e$803){var b$804;
var init$1603 = [0,[0,0]];
var v$1604 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1606 = [null];
b$804 = [basis$0Option$1.map$65(function(v$1607){return [v$1607];
},init$1603),v$1606,v$1604];
(js$0Js$1.onMouseMoveElem$336(e$803))(function(v$809){(newValue$232(function(v$1188){return (v$1188[0][0] == v$1188[1][0])?(v$1188[0][1] == v$1188[1][1]):false;
},b$804))(v$809);
return eval$209(0);
});
return b$804;
};
rwp$0Rwp$1.mouse_doc$812 = function(d$815){var b$816;
var init$1609 = [0,[0,0]];
var v$1610 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1612 = [null];
b$816 = [basis$0Option$1.map$65(function(v$1613){return [v$1613];
},init$1609),v$1612,v$1610];
(js$0Js$1.onMouseMove$272(d$815))(function(v$821){(newValue$232(function(v$1194){return (v$1194[0][0] == v$1194[1][0])?(v$1194[0][1] == v$1194[1][1]):false;
},b$816))(v$821);
return eval$209(0);
});
return b$816;
};
rwp$0Rwp$1.mouse$824 = function(v$826){return rwp$0Rwp$1.mouse_doc$812(js$0Js$1.document$84);
};
rwp$0Rwp$1.click_elem$827 = function(v$1197,e$830,a$833){var t$834;
var init$1615 = [1];
var v$1616 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1618 = [null];
t$834 = [basis$0Option$1.map$65(function(v$1619){return [v$1619];
},init$1615),v$1618,v$1616];
js$0Js$1.installEventHandler$193(e$830,4,function(v$838){(newValue$232(v$1197,t$834))(a$833);
eval$209(0);
return true;
});
return t$834;
};
rwp$0Rwp$1.click$843 = function(v$1198,id$846,a$849){var v$854 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$84,id$846);
switch (v$854[0]) { case 0: {var v$856 = v$854[1];
return rwp$0Rwp$1.click_elem$827(v$1198,v$856,a$849);
 break; }default: {throw [basis$0Initial$1.en$Fail$50,("click: element with id=" + id$846) + " not in dom"];
} };
};
rwp$0Rwp$1.changes$857 = function(v$1199,b$860){var t$861;
var init$1628 = [1];
var v$1629 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1631 = [null];
t$861 = [basis$0Option$1.map$65(function(v$1632){return [v$1632];
},init$1628),v$1631,v$1629];
var v$1634 = b$860[1];
(v$1634[0] = [[0,newValue$232(v$1199,t$861)],v$1634[0]],0);
return t$861;
};
rwp$0Rwp$1.hold$864 = function(v$1200,a$867,e$870){var b$871;
var init$1638 = [0,a$867];
var v$1639 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1641 = [null];
b$871 = [basis$0Option$1.map$65(function(v$1642){return [v$1642];
},init$1638),v$1641,v$1639];
var v$1644 = e$870[1];
(v$1644[0] = [[0,newValue$232(v$1200,b$871)],v$1644[0]],0);
return b$871;
};
rwp$0Rwp$1.fold$874 = function(v$1201,v$1202,f$877,x$880,a$883){var t$884 = [x$880];
var es$885;
var init$1648 = [1];
var v$1649 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1651 = [null];
es$885 = [basis$0Option$1.map$65(function(v$1652){return [v$1652];
},init$1648),v$1651,v$1649];
var v$1654 = a$883[1];
(v$1654[0] = [[0,function(v$2181){var r$2182 = f$877([v$2181,t$884[0]]);
(t$884[0] = r$2182,0);
return (newValue$232(v$1202,es$885))(r$2182);
}],v$1654[0]],0);
return es$885;
};
rwp$0Rwp$1.empty$894 = function(v$1203,v$896){var init$1660 = [1];
var v$1661 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1663 = [null];
return [basis$0Option$1.map$65(function(v$1664){return [v$1664];
},init$1660),v$1663,v$1661];
};
rwp$0Rwp$1.const$897 = function(v$1204,a$900){var init$1666 = [0,a$900];
var v$1667 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1669 = [null];
return [basis$0Option$1.map$65(function(v$1670){return [v$1670];
},init$1666),v$1669,v$1667];
};
rwp$0Rwp$1.poll$901 = function(v$1205,f$904,ms$907){var b$908;
var init$1672 = [0,f$904(0)];
var v$1673 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1675 = [null];
b$908 = [basis$0Option$1.map$65(function(v$1676){return [v$1676];
},init$1672),v$1675,v$1673];
(function(i,f){return setInterval(f,i);})(ms$907,function(v$911){(newValue$232(v$1205,b$908))(f$904(v$911));
return eval$209(0);
});
return b$908;
};
rwp$0Rwp$1.timer$914 = function(m$917){return rwp$0Rwp$1.poll$901(function(v$1206){return (v$1206[0][0] == v$1206[1][0])?(v$1206[0][1] == v$1206[1][1]):false;
},function(v$1679){return SmlPrims.getrealtime();
},m$917);
};
var addListener$$$922 = function(v$1209,b$925,f$928){var li$929;
var v$1681 = b$925[1];
var lid$1683 = [c$151[0],(c$151[0] = (SmlPrims.chk_ovf_i32(c$151[0] + 1)),0)][0];
(v$1681[0] = [[lid$1683,f$928],v$1681[0]],0);
li$929 = lid$1683;
return function(v$931){var v$1687 = b$925[1];
var fix$2383 = {};
fix$2383.$remove = function(v$1690){if (v$1690 == null) {return [null,false];
} else {var v$1691 = v$1690;
var v$1692 = v$1691[0];
var v$1693 = v$1692[0];
var v$1694 = v$1691[1];
if (v$1693 == li$929) {return [v$1694,true];
} else {var v$1695 = fix$2383.$remove(v$1694);
return [[v$1692,v$1695[0]],v$1695[1]];
};
};
};
var remove$1689 = fix$2383.$remove;
var v$1696 = remove$1689(v$1687[0]);
var v$1697 = v$1696[0];
var v$1698 = v$1696[1];
v$1698?(v$1687[0] = v$1697,0):0;
return v$1698;
};
};
rwp$0Rwp$1.flatten$932 = function(v$1210,a$935){var v$1702 = a$935[0];
switch (v$1702[0]) { case 0: {v$1702[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
var n$937;
var init$1704;
var t$2384;
var v$1714 = a$935[0];
var b$1710;
switch (v$1714[0]) { case 0: {b$1710 = v$1714[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
var v$1711 = b$1710[0];
switch (v$1711[0]) { case 0: {t$2384 = v$1711[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
init$1704 = [0,t$2384];
var v$1705 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1707 = [null];
n$937 = [basis$0Option$1.map$65(function(v$1708){return [v$1708];
},init$1704),v$1707,v$1705];
var remLi$938;
var t$2388;
var t$2387 = addListener$$$922;
var t$2386 = v$1210;
var t$2385;
var v$1717 = a$935[0];
switch (v$1717[0]) { case 0: {t$2385 = v$1717[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
t$2388 = (t$2387(t$2386,t$2385,newValue$232(v$1210,n$937)));
remLi$938 = [t$2388];
var v$1719 = a$935[1];
(v$1719[0] = [[0,function(b$2183){remLi$938[0](0);
(remLi$938[0] = (addListener$$$922(v$1210,b$2183,newValue$232(v$1210,n$937))),0);
var t$2390 = newValue$232(v$1210,n$937);
var t$2389;
var v$2184 = b$2183[0];
switch (v$2184[0]) { case 0: {t$2389 = v$2184[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
return t$2390(t$2389);
}],v$1719[0]],0);
return n$937;
};
rwp$0Rwp$1.arr$948 = function(v$1223,v$1224,f$951,b0$954){var b$955;
var init$1728;
var v$960 = b0$954[0];
switch (v$960[0]) { case 0: {var v$962 = v$960[1];
var v$956 = v$962[0];
init$1728 = [0,f$951(v$956)];
 break; }default: {init$1728 = [1];
} };
var v$1729 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1731 = [null];
b$955 = [basis$0Option$1.map$65(function(v$1732){return [v$1732];
},init$1728),v$1731,v$1729];
var v$1734 = b0$954[1];
var t$2396 = v$1734;
var t$2395;
var t$2394;
var t$2393;
var t$2392 = 0;
var t$2391;
var v$2074 = newValue$232(v$1224,b$955);
t$2391 = (function(x$1740){return v$2074(f$951(x$1740));
});
t$2393 = [t$2392,t$2391];
t$2394 = [t$2393,v$1734[0]];
t$2395 = t$2394;
(t$2396[0] = t$2395,0);
return b$955;
};
var fix$2397 = {};
fix$2397.$list = function(v$1225,v$972){if (v$972 == null) {var a$1742 = null;
var init$1743 = [0,a$1742];
var v$1744 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1746 = [null];
return [basis$0Option$1.map$65(function(v$1747){return [v$1747];
},init$1743),v$1746,v$1744];
} else {var v$980 = v$972;
var v$981 = v$980[0];
var v$982 = v$980[1];
var t$2464 = rwp$0Rwp$1.arr$948;
var t$2463 = function(v$1226){if (v$1225([v$1226[0][0],v$1226[1][0]])) {var fix$2460 = {};
fix$2460.$eq_list = function(v$1759,v$1760){lab$eq_list: while (true) {if (v$1759 == null) {return (v$1760 == null)?true:false;
} else {if (v$1760 == null) {return false;
} else {var v$1761 = v$1759;
var v$1762 = v$1760;
if (v$1225([v$1761[0],v$1762[0]])) {var t$2461 = v$1761[1];
var t$2462 = v$1762[1];
var v$1759 = t$2461;
var v$1760 = t$2462;
continue lab$eq_list;
} else {return false;
};
};
};
};
};
var eq_list$1757 = fix$2460.$eq_list;
return eq_list$1757(v$1226[0][1],v$1226[1][1]);
} else {return false;
};
};
var t$2459;
var fix$2456 = {};
fix$2456.$eq_list = function(v$1767,v$1768){lab$eq_list: while (true) {if (v$1767 == null) {return (v$1768 == null)?true:false;
} else {if (v$1768 == null) {return false;
} else {var v$1769 = v$1767;
var v$1770 = v$1768;
if (v$1225([v$1769[0],v$1770[0]])) {var t$2457 = v$1769[1];
var t$2458 = v$1770[1];
var v$1767 = t$2457;
var v$1768 = t$2458;
continue lab$eq_list;
} else {return false;
};
};
};
};
};
var eq_list$1765 = fix$2456.$eq_list;
t$2459 = (function(v$2283){return eq_list$1765(v$2283[0],v$2283[1]);
});
var t$2455 = function(v$979){return v$979;
};
var t$2449;
var v$2290 = [v$981,fix$2397.$list(v$1225,v$982)];
var t$2454 = pairT$421;
var t$2453;
var fix$2450 = {};
fix$2450.$eq_list = function(v$2325,v$2326){lab$eq_list: while (true) {if (v$2325 == null) {return (v$2326 == null)?true:false;
} else {if (v$2326 == null) {return false;
} else {var v$2327 = v$2325;
var v$2328 = v$2326;
if (v$1225([v$2327[0],v$2328[0]])) {var t$2451 = v$2327[1];
var t$2452 = v$2328[1];
var v$2325 = t$2451;
var v$2326 = t$2452;
continue lab$eq_list;
} else {return false;
};
};
};
};
};
var eq_list$2324 = fix$2450.$eq_list;
t$2453 = (function(v$2329){return eq_list$2324(v$2329[0],v$2329[1]);
});
t$2449 = (t$2454(t$2453,v$1225,v$2290));
return t$2464(t$2463,t$2459,t$2455,t$2449);
};
};
rwp$0Rwp$1.list$969 = fix$2397.$list;
rwp$0Rwp$1.fst$983 = function(v$1227,v$1228,v$1229,f$986,p$989){var t$2417 = pairT$421;
var t$2416 = v$1228;
var t$2415 = v$1229;
var t$2414;
var t$2413;
var t$2412 = f$986;
var t$2405;
var v1opt$1785;
var v$1786 = p$989[0];
switch (v$1786[0]) { case 0: {var v$1787 = v$1786[1];
var v$1788 = v$1787[0][0];
v1opt$1785 = [0,v$1788];
 break; }default: {v1opt$1785 = [1];
} };
var e$1790;
var v$1791 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1793 = [null];
e$1790 = [basis$0Option$1.map$65(function(v$1794){return [v$1794];
},v1opt$1785),v$1793,v$1791];
var v$1796 = p$989[1];
var t$2411 = v$1796;
var t$2410;
var t$2409;
var t$2408;
var t$2407 = 0;
var t$2406;
var v$2088 = newValue$232(v$1227,e$1790);
t$2406 = (function(x$1802){return v$2088(x$1802[0]);
});
t$2408 = [t$2407,t$2406];
t$2409 = [t$2408,v$1796[0]];
t$2410 = t$2409;
(t$2411[0] = t$2410,0);
t$2405 = e$1790;
t$2413 = (t$2412(t$2405));
var t$2398;
var v2opt$1807;
var v$1808 = p$989[0];
switch (v$1808[0]) { case 0: {var v$1809 = v$1808[1];
var v$1810 = v$1809[0][1];
v2opt$1807 = [0,v$1810];
 break; }default: {v2opt$1807 = [1];
} };
var e$1812;
var v$1813 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1815 = [null];
e$1812 = [basis$0Option$1.map$65(function(v$1816){return [v$1816];
},v2opt$1807),v$1815,v$1813];
var v$1818 = p$989[1];
var t$2404 = v$1818;
var t$2403;
var t$2402;
var t$2401;
var t$2400 = 0;
var t$2399;
var v$2094 = newValue$232(v$1228,e$1812);
t$2399 = (function(x$1824){return v$2094(x$1824[1]);
});
t$2401 = [t$2400,t$2399];
t$2402 = [t$2401,v$1818[0]];
t$2403 = t$2402;
(t$2404[0] = t$2403,0);
t$2398 = e$1812;
t$2414 = [t$2413,t$2398];
return t$2417(t$2416,t$2415,t$2414);
};
rwp$0Rwp$1.s$lll$990 = function(v$995,v$996){return function(x$1829){return v$996(v$995(x$1829));
};
};
rwp$0Rwp$1.snd$997 = function(v$1230,v$1231,v$1232,f$1000){return function(x$1836){var v$2344;
var v$2357 = rwp$0Rwp$1.arr$948(function(v$2358){if (v$1232([v$2358[0][0],v$2358[1][0]])) {return v$1230([v$2358[0][1],v$2358[1][1]]);
} else {return false;
};
},function(v$2359){if (v$1230([v$2359[0][0],v$2359[1][0]])) {return v$1232([v$2359[0][1],v$2359[1][1]]);
} else {return false;
};
},function(v$2360){return [v$2360[1],v$2360[0]];
},x$1836);
v$2344 = (rwp$0Rwp$1.fst$983(v$1230,v$1232,v$1231,f$1000,v$2357));
return rwp$0Rwp$1.arr$948(function(v$2345){if (v$1231([v$2345[0][0],v$2345[1][0]])) {return v$1232([v$2345[0][1],v$2345[1][1]]);
} else {return false;
};
},function(v$2346){if (v$1232([v$2346[0][0],v$2346[1][0]])) {return v$1231([v$2346[0][1],v$2346[1][1]]);
} else {return false;
};
},function(v$2347){return [v$2347[1],v$2347[0]];
},v$2344);
};
};
rwp$0Rwp$1.s$ttt$1008 = function(v$1237,v$1238,v$1239,v$1240,v$1012){var v$1013 = v$1012[0];
var v$1014 = v$1012[1];
var v$2109 = rwp$0Rwp$1.snd$997(v$1237,v$1239,v$1240,v$1014);
return function(x$1852){return v$2109(rwp$0Rwp$1.fst$983(v$1238,v$1237,v$1240,v$1013,x$1852));
};
};
rwp$0Rwp$1.s$ccc$1015 = function(v$1241,v$1242,v$1243,v$1019){var v$1023 = v$1019[0];
var v$1024 = v$1019[1];
var v$2113;
var v$2197 = rwp$0Rwp$1.snd$997(v$1241,v$1242,v$1243,v$1024);
v$2113 = (function(x$2198){return v$2197(rwp$0Rwp$1.fst$983(v$1241,v$1241,v$1243,v$1023,x$2198));
});
return function(x$1859){return v$2113(rwp$0Rwp$1.arr$948(v$1241,function(v$2353){if (v$1241([v$2353[0][0],v$2353[1][0]])) {return v$1241([v$2353[0][1],v$2353[1][1]]);
} else {return false;
};
},function(b$2354){return [b$2354,b$2354];
},x$1859));
};
};
rwp$0Rwp$1.iff$1025 = function(v$1245,v$1030){var v$1061 = v$1030[0];
var v$1062 = v$1030[1];
var v$1063 = v$1030[2];
var init$1031;
var t$2418;
var v$1862 = v$1061[0];
switch (v$1862[0]) { case 0: {t$2418 = v$1862[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
if (t$2418) {var v$1865 = v$1062[0];
switch (v$1865[0]) { case 0: {init$1031 = v$1865[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
} else {var v$1868 = v$1063[0];
switch (v$1868[0]) { case 0: {init$1031 = v$1868[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
};
var b$1036;
var init$1870 = [0,init$1031];
var v$1871 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$1873 = [null];
b$1036 = [basis$0Option$1.map$65(function(v$1874){return [v$1874];
},init$1870),v$1873,v$1871];
var v$2204 = v$1062[1];
(v$2204[0] = [[0,function(a$2205){var t$2421;
var v$2206 = v$1061[0];
switch (v$2206[0]) { case 0: {t$2421 = v$2206[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
if (t$2421) {var t$2420 = newValue$232(v$1245,b$1036);
var t$2419;
var v$2207 = v$1062[0];
switch (v$2207[0]) { case 0: {t$2419 = v$2207[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
return t$2420(t$2419);
} else {return 0;
};
}],v$2204[0]],0);
var v$2210 = v$1063[1];
(v$2210[0] = [[0,function(a$2211){var t$2425;
var t$2424;
var v$2212 = v$1061[0];
switch (v$2212[0]) { case 0: {t$2424 = v$2212[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
t$2425 = (t$2424?false:true);
if (t$2425) {var t$2423 = newValue$232(v$1245,b$1036);
var t$2422;
var v$2213 = v$1063[0];
switch (v$2213[0]) { case 0: {t$2422 = v$2213[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
return t$2423(t$2422);
} else {return 0;
};
}],v$2210[0]],0);
var v$1887 = v$1061[1];
(v$1887[0] = [[0,function(t$2214){if (t$2214) {var t$2427 = newValue$232(v$1245,b$1036);
var t$2426;
var v$2215 = v$1062[0];
switch (v$2215[0]) { case 0: {t$2426 = v$2215[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
return t$2427(t$2426);
} else {var t$2429 = newValue$232(v$1245,b$1036);
var t$2428;
var v$2216 = v$1063[0];
switch (v$2216[0]) { case 0: {t$2428 = v$2216[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
return t$2429(t$2428);
};
}],v$1887[0]],0);
return b$1036;
};
rwp$0Rwp$1.when$1064 = function(v$1248,v$1068){var v$1083 = v$1068[0];
var v$1084 = v$1068[1];
var init$1069;
var v$1898 = v$1084[0];
switch (v$1898[0]) { case 0: {init$1069 = v$1898[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
var e$2219;
var t$2435 = rwp$0Rwp$1.fold$874;
var t$2434 = function(v$1249){if (v$1249[0][0] == v$1249[1][0]) {return v$1248([v$1249[0][1],v$1249[1][1]]);
} else {return false;
};
};
var t$2433 = v$1248;
var t$2432 = function(v$1074){var v$1079 = v$1074[0];
var v$1080 = v$1079[0];
var v$1081 = v$1079[1];
var v$1082 = v$1074[1];
return v$1080?v$1081:v$1082;
};
var t$2431 = init$1069;
var t$2430;
var b$2227 = pairT$421(v$1248,function(v$2234){return v$2234[0] == v$2234[1];
},[v$1083,v$1084]);
var t$2228;
var init$2229 = [1];
var v$2230 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$2231 = [null];
t$2228 = [basis$0Option$1.map$65(function(v$2232){return [v$2232];
},init$2229),v$2231,v$2230];
var v$2233 = b$2227[1];
(v$2233[0] = [[0,newValue$232(function(v$2275){if (v$2275[0][0] == v$2275[1][0]) {return v$1248([v$2275[0][1],v$2275[1][1]]);
} else {return false;
};
},t$2228)],v$2233[0]],0);
t$2430 = t$2228;
e$2219 = (t$2435(t$2434,t$2433,t$2432,t$2431,t$2430));
var b$2220;
var init$2221 = [0,init$1069];
var v$2222 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$2223 = [null];
b$2220 = [basis$0Option$1.map$65(function(v$2224){return [v$2224];
},init$2221),v$2223,v$2222];
var v$2225 = e$2219[1];
(v$2225[0] = [[0,newValue$232(v$1248,b$2220)],v$2225[0]],0);
return b$2220;
};
rwp$0Rwp$1.until$1085 = function(v$1254,v$1089){var v$1121 = v$1089[0];
var v$1122 = v$1089[1];
var init$1090;
var t$2437;
var v$1903 = v$1122[0];
switch (v$1903[0]) { case 0: {t$2437 = v$1903[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
var t$2436;
var v$1906 = v$1121[0];
switch (v$1906[0]) { case 0: {t$2436 = v$1906[1][0];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"rwp.current.impossible"];
} };
init$1090 = [t$2437,t$2436];
var t$2448 = rwp$0Rwp$1.arr$948;
var t$2447 = function(v$1256){return (v$1254([v$1256[0][0],v$1256[1][0]]))?(v$1256[0][1] == v$1256[1][1]):false;
};
var t$2446 = v$1254;
var t$2445 = function(v$1093){return v$1093[0];
};
var t$2444;
var e$2237;
var t$2443 = rwp$0Rwp$1.fold$874;
var t$2442 = function(v$1260){if (v$1260[0][0] == v$1260[1][0]) {return v$1254([v$1260[0][1],v$1260[1][1]]);
} else {return false;
};
};
var t$2441 = function(v$1262){return (v$1254([v$1262[0][0],v$1262[1][0]]))?(v$1262[0][1] == v$1262[1][1]):false;
};
var t$2440 = function(v$1106){var v$1107 = v$1106[1];
if (v$1107[1]) {return v$1107;
} else {var v$1117 = v$1106[0];
var v$1118 = v$1117[0];
var v$1119 = v$1117[1];
return v$1118?[v$1119,true]:[v$1119,false];
};
};
var t$2439 = init$1090;
var t$2438;
var b$2245 = pairT$421(v$1254,function(v$2252){return v$2252[0] == v$2252[1];
},[v$1121,v$1122]);
var t$2246;
var init$2247 = [1];
var v$2248 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$2249 = [null];
t$2246 = [basis$0Option$1.map$65(function(v$2250){return [v$2250];
},init$2247),v$2249,v$2248];
var v$2251 = b$2245[1];
(v$2251[0] = [[0,newValue$232(function(v$2276){if (v$2276[0][0] == v$2276[1][0]) {return v$1254([v$2276[0][1],v$2276[1][1]]);
} else {return false;
};
},t$2246)],v$2251[0]],0);
t$2438 = t$2246;
e$2237 = (t$2443(t$2442,t$2441,t$2440,t$2439,t$2438));
var b$2238;
var init$2239 = [0,init$1090];
var v$2240 = [c$138[0],(c$138[0] = (SmlPrims.chk_ovf_i32(c$138[0] + 1)),0)][0];
var v$2241 = [null];
b$2238 = [basis$0Option$1.map$65(function(v$2242){return [v$2242];
},init$2239),v$2241,v$2240];
var v$2243 = e$2237[1];
(v$2243[0] = [[0,newValue$232(function(v$2277){return (v$1254([v$2277[0][0],v$2277[1][0]]))?(v$2277[0][1] == v$2277[1][1]):false;
},b$2238)],v$2243[0]],0);
t$2444 = b$2238;
return t$2448(t$2447,t$2446,t$2445,t$2444);
};
return 0;
})();
