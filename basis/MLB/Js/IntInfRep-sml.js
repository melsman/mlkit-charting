if ((typeof(basis$0IntInfRep$1)) == "undefined") {basis$0IntInfRep$1 = {};
};
(function(){var fix$2188 = {};
fix$2188.$addOne = function(v$277){if (v$277 == null) {return [1,null];
} else {var v$289 = v$277;
var v$290 = v$289[0];
var v$291 = v$289[1];
var c$284 = SmlPrims.chk_ovf_i31((SmlPrims.chk_ovf_i31((-1073741824) + v$290)) + 1);
return (c$284 < 0)?[SmlPrims.chk_ovf_i31(c$284 - (-1073741824)),v$291]:[c$284,fix$2188.$addOne(v$291)];
};
};
var addOne$274 = fix$2188.$addOne;
var fix$2189 = {};
fix$2189.$add = function(v$310,v$2155){if (v$310 == null) {return v$2155;
} else {if (v$2155 == null) {return v$310;
} else {var v$313 = v$310;
var v$314 = v$313[0];
var v$315 = v$313[1];
var v$316 = v$2155;
var v$317 = v$316[0];
var v$318 = v$316[1];
return fix$2189.$addd(SmlPrims.chk_ovf_i31((SmlPrims.chk_ovf_i31((-1073741824) + v$314)) + v$317),v$315,v$318);
};
};
};
fix$2189.$addd = function(v$328,v$329,v$330){return (v$328 < 0)?[SmlPrims.chk_ovf_i31(v$328 - (-1073741824)),fix$2189.$add(v$329,v$330)]:[v$328,fix$2189.$addc(v$329,v$330)];
};
fix$2189.$addc = function(v$2154,v$346){if (v$346 == null) {return addOne$274(v$2154);
} else {if (v$2154 == null) {return addOne$274(v$346);
} else {var v$349 = v$2154;
var v$350 = v$349[0];
var v$351 = v$349[1];
var v$352 = v$346;
var v$353 = v$352[0];
var v$354 = v$352[1];
return fix$2189.$addd(SmlPrims.chk_ovf_i31((SmlPrims.chk_ovf_i31((SmlPrims.chk_ovf_i31((-1073741824) + v$350)) + v$353)) + 1),v$351,v$354);
};
};
};
var add$294 = fix$2189.$add;
var addd$293 = fix$2189.$addd;
var addc$292 = fix$2189.$addc;
var fix$2190 = {};
fix$2190.$subtOne = function(v$358){if (v$358 == null) {throw [basis$0Initial$1.en$Fail$54,""];
} else {var v$372 = v$358;
switch (v$372[0]) { case 0: {var v$379 = v$372[1];
return [1073741823,fix$2190.$subtOne(v$379)];
 break; }case 1: {if (v$372[1] == null) {return null;
} else {var v$1577 = v$358;
var v$1578 = v$1577[0];
var v$1579 = v$1577[1];
return [SmlPrims.chk_ovf_i31(v$1578 - 1),v$1579];
};
 break; }default: {var v$1581 = v$358;
var v$1582 = v$1581[0];
var v$1583 = v$1581[1];
return [SmlPrims.chk_ovf_i31(v$1582 - 1),v$1583];
} };
};
};
var subtOne$355 = fix$2190.$subtOne;
var en$Negative$380 = new String("Negative");
var exn$Negative$380 = [en$Negative$380];
var fix$2191 = {};
fix$2191.$subt = function(v$2157,v$415){if (v$415 == null) {return v$2157;
} else {if (v$2157 == null) {throw exn$Negative$380;
} else {var v$418 = v$2157;
var v$419 = v$418[0];
var v$420 = v$418[1];
var v$421 = v$415;
var v$422 = v$421[0];
var v$423 = v$421[1];
return fix$2191.$subd(SmlPrims.chk_ovf_i31(v$419 - v$422),v$420,v$423);
};
};
};
fix$2191.$subb = function(v$440,v$2156){if (v$440 == null) {throw exn$Negative$380;
} else {if (v$2156 == null) {var v$443 = v$440;
var v$444 = v$443[0];
var v$445 = v$443[1];
return fix$2191.$subd(SmlPrims.chk_ovf_i31(v$444 - 1),v$445,null);
} else {var v$446 = v$440;
var v$447 = v$446[0];
var v$448 = v$446[1];
var v$449 = v$2156;
var v$450 = v$449[0];
var v$451 = v$449[1];
return fix$2191.$subd(SmlPrims.chk_ovf_i31((SmlPrims.chk_ovf_i31(v$447 - v$450)) - 1),v$448,v$451);
};
};
};
fix$2191.$subd = function(v$461,v$462,v$463){if (v$461 >= 0) {var v$2033 = fix$2191.$subt(v$462,v$463);
switch (v$461) { case 0: {return (v$2033 == null)?null:[v$461,v$2033];
 break; }default: {return [v$461,v$2033];
} };
} else {var v$2034 = SmlPrims.chk_ovf_i31(v$461 - (-1073741824));
var v$2035 = fix$2191.$subb(v$462,v$463);
switch (v$2034) { case 0: {return (v$2035 == null)?null:[v$2034,v$2035];
 break; }default: {return [v$2034,v$2035];
} };
};
};
var subt$399 = fix$2191.$subt;
var subb$398 = fix$2191.$subb;
var subd$397 = fix$2191.$subd;
var muld$498 = function(v$2158,v$512){switch (v$512) { case 0: {return null;
 break; }case 1: {return v$2158;
 break; }default: {var fix$2192 = {};
fix$2192.$muldc = function(v$529,v$2159){if (v$529 == null) {switch (v$2159) { case 0: {return null;
 break; }default: {return [v$2159,null];
} };
} else {var v$542 = v$529;
var v$543 = v$542[0];
var v$544 = v$542[1];
var v$539;
var v$1606;
var w$2127 = (SmlPrims.i32_to_w32(v$543)) & 2147483647;
var t$2201;
var t$2200;
var t$2199;
var v$2128 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2199 = ((v$2128 >= 31)?((w$2127 <= 1073741823)?0:2147483647):((w$2127 & -0x40000000)?(((w$2127 | 2147483648) >> v$2128) & 2147483647):(w$2127 >> v$2128)));
t$2200 = (SmlPrims.w31_to_i32_X(t$2199));
t$2201 = (SmlPrims.chk_ovf_i31(t$2200));
var t$2198;
var t$2197;
var t$2196;
var t$2195 = w$2127;
var t$2194;
var t$2193;
var v$2129 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2193 = ((v$2129 >= 31)?0:((1 << (v$2129 & 31)) & 2147483647));
t$2194 = ((t$2193 - 1) & 2147483647);
t$2196 = (t$2195 & t$2194);
t$2197 = (SmlPrims.w31_to_i32_X(t$2196));
t$2198 = (SmlPrims.chk_ovf_i31(t$2197));
v$1606 = [t$2201,t$2198];
var v$1607 = v$1606[0];
var v$1608 = v$1606[1];
var v$1609;
var w$2131 = (SmlPrims.i32_to_w32(v$512)) & 2147483647;
var t$2210;
var t$2209;
var t$2208;
var v$2132 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2208 = ((v$2132 >= 31)?((w$2131 <= 1073741823)?0:2147483647):((w$2131 & -0x40000000)?(((w$2131 | 2147483648) >> v$2132) & 2147483647):(w$2131 >> v$2132)));
t$2209 = (SmlPrims.w31_to_i32_X(t$2208));
t$2210 = (SmlPrims.chk_ovf_i31(t$2209));
var t$2207;
var t$2206;
var t$2205;
var t$2204 = w$2131;
var t$2203;
var t$2202;
var v$2133 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2202 = ((v$2133 >= 31)?0:((1 << (v$2133 & 31)) & 2147483647));
t$2203 = ((t$2202 - 1) & 2147483647);
t$2205 = (t$2204 & t$2203);
t$2206 = (SmlPrims.w31_to_i32_X(t$2205));
t$2207 = (SmlPrims.chk_ovf_i31(t$2206));
v$1609 = [t$2210,t$2207];
var v$1610 = v$1609[0];
var v$1611 = v$1609[1];
var x$1612 = SmlPrims.chk_ovf_i31(v$1607 * v$1610);
var y$1613 = SmlPrims.chk_ovf_i31((SmlPrims.chk_ovf_i31(v$1607 - v$1608)) * (SmlPrims.chk_ovf_i31(v$1610 - v$1611)));
var z$1614 = SmlPrims.chk_ovf_i31(v$1608 * v$1611);
var v$1615;
var w$2135 = (SmlPrims.i32_to_w32(z$1614)) & 2147483647;
var t$2219;
var t$2218;
var t$2217;
var v$2136 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2217 = ((v$2136 >= 31)?((w$2135 <= 1073741823)?0:2147483647):((w$2135 & -0x40000000)?(((w$2135 | 2147483648) >> v$2136) & 2147483647):(w$2135 >> v$2136)));
t$2218 = (SmlPrims.w31_to_i32_X(t$2217));
t$2219 = (SmlPrims.chk_ovf_i31(t$2218));
var t$2216;
var t$2215;
var t$2214;
var t$2213 = w$2135;
var t$2212;
var t$2211;
var v$2137 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2211 = ((v$2137 >= 31)?0:((1 << (v$2137 & 31)) & 2147483647));
t$2212 = ((t$2211 - 1) & 2147483647);
t$2214 = (t$2213 & t$2212);
t$2215 = (SmlPrims.w31_to_i32_X(t$2214));
t$2216 = (SmlPrims.chk_ovf_i31(t$2215));
v$1615 = [t$2219,t$2216];
var v$1616 = v$1615[0];
var v$1617 = v$1615[1];
var v$1618;
var i$2138 = SmlPrims.chk_ovf_i31((SmlPrims.chk_ovf_i31((SmlPrims.chk_ovf_i31((SmlPrims.chk_ovf_i31((-1073741824) + x$1612)) + z$1614)) - y$1613)) + v$1616);
var w$2139 = (SmlPrims.i32_to_w32(i$2138)) & 2147483647;
var t$2228;
var t$2227;
var t$2226;
var v$2140 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2226 = ((v$2140 >= 31)?((w$2139 <= 1073741823)?0:2147483647):((w$2139 & -0x40000000)?(((w$2139 | 2147483648) >> v$2140) & 2147483647):(w$2139 >> v$2140)));
t$2227 = (SmlPrims.w31_to_i32_X(t$2226));
t$2228 = (SmlPrims.chk_ovf_i31(t$2227));
var t$2225;
var t$2224;
var t$2223;
var t$2222 = w$2139;
var t$2221;
var t$2220;
var v$2141 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2220 = ((v$2141 >= 31)?0:((1 << (v$2141 & 31)) & 2147483647));
t$2221 = ((t$2220 - 1) & 2147483647);
t$2223 = (t$2222 & t$2221);
t$2224 = (SmlPrims.w31_to_i32_X(t$2223));
t$2225 = (SmlPrims.chk_ovf_i31(t$2224));
v$1618 = [t$2228,t$2225];
var v$1619 = v$1618[0];
var v$1620 = v$1618[1];
var t$2237;
var t$2236 = SmlPrims.chk_ovf_i31(x$1612 + v$1619);
var t$2235;
var t$2234;
var t$2233;
var v$2039 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2233 = ((v$2039 >= 31)?0:((1 << (v$2039 & 31)) & 2147483647));
t$2234 = (SmlPrims.w31_to_i32_X(t$2233));
t$2235 = (SmlPrims.chk_ovf_i31(t$2234));
t$2237 = (SmlPrims.chk_ovf_i31(t$2236 + t$2235));
var t$2232;
var t$2231;
var t$2230;
var t$2229;
var v$2042 = (SmlPrims.i32_to_w32(v$1620)) & 2147483647;
var v$2043 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2229 = ((v$2043 >= 31)?0:((v$2042 << (v$2043 & 31)) & 2147483647));
t$2230 = (SmlPrims.w31_to_i32_X(t$2229));
t$2231 = (SmlPrims.chk_ovf_i31(t$2230));
t$2232 = (SmlPrims.chk_ovf_i31(t$2231 + v$1617));
v$539 = [t$2237,t$2232];
var v$540 = v$539[0];
var v$541 = v$539[1];
var l1$534 = SmlPrims.chk_ovf_i31((SmlPrims.chk_ovf_i31(v$541 + (-1073741824))) + v$2159);
return (l1$534 >= 0)?[l1$534,fix$2192.$muldc(v$544,SmlPrims.chk_ovf_i31(v$540 + 1))]:[SmlPrims.chk_ovf_i31(l1$534 - (-1073741824)),fix$2192.$muldc(v$544,v$540)];
};
};
var muldc$515 = fix$2192.$muldc;
return muldc$515(v$2158,0);
} };
};
var fix$2238 = {};
fix$2238.$mult = function(v$2160,v$566){if (v$566 == null) {return null;
} else {var v$568 = v$566;
var v$569 = v$568[1];
if (v$569 == null) {var v$571 = v$568[0];
return muld$498(v$2160,v$571);
} else {switch (v$568[0]) { case 0: {var v$2047 = fix$2238.$mult(v$2160,v$569);
switch (0) { case 0: {return (v$2047 == null)?null:[0,v$2047];
 break; }default: {return [0,v$2047];
} };
 break; }default: {var fix$2343 = {};
fix$2343.$muln = function(v$577){if (v$577 == null) {return null;
} else {var v$584 = v$577;
var v$585 = v$584[0];
var v$586 = v$584[1];
var t$2346 = add$294;
var t$2345 = muld$498(v$566,v$585);
var t$2344;
var v$2049 = fix$2343.$muln(v$586);
switch (0) { case 0: {t$2344 = ((v$2049 == null)?null:[0,v$2049]);
 break; }default: {t$2344 = [0,v$2049];
} };
return t$2346(t$2345,t$2344);
};
};
var muln$574 = fix$2343.$muln;
return muln$574(v$2160);
} };
};
};
};
var mult$547 = fix$2238.$mult;
var divmod2$603 = function(v$656,v$2161){var v$657 = v$656[0];
var v$658 = v$656[1];
var v$653;
var w$2143 = (SmlPrims.i32_to_w32(v$658)) & 2147483647;
var t$2247;
var t$2246;
var t$2245;
var v$2144 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2245 = ((v$2144 >= 31)?((w$2143 <= 1073741823)?0:2147483647):((w$2143 & -0x40000000)?(((w$2143 | 2147483648) >> v$2144) & 2147483647):(w$2143 >> v$2144)));
t$2246 = (SmlPrims.w31_to_i32_X(t$2245));
t$2247 = (SmlPrims.chk_ovf_i31(t$2246));
var t$2244;
var t$2243;
var t$2242;
var t$2241 = w$2143;
var t$2240;
var t$2239;
var v$2145 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2239 = ((v$2145 >= 31)?0:((1 << (v$2145 & 31)) & 2147483647));
t$2240 = ((t$2239 - 1) & 2147483647);
t$2242 = (t$2241 & t$2240);
t$2243 = (SmlPrims.w31_to_i32_X(t$2242));
t$2244 = (SmlPrims.chk_ovf_i31(t$2243));
v$653 = [t$2247,t$2244];
var v$654 = v$653[0];
var v$655 = v$653[1];
var v$650;
var w$2147 = (SmlPrims.i32_to_w32(v$2161)) & 2147483647;
var t$2256;
var t$2255;
var t$2254;
var v$2148 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2254 = ((v$2148 >= 31)?((w$2147 <= 1073741823)?0:2147483647):((w$2147 & -0x40000000)?(((w$2147 | 2147483648) >> v$2148) & 2147483647):(w$2147 >> v$2148)));
t$2255 = (SmlPrims.w31_to_i32_X(t$2254));
t$2256 = (SmlPrims.chk_ovf_i31(t$2255));
var t$2253;
var t$2252;
var t$2251;
var t$2250 = w$2147;
var t$2249;
var t$2248;
var v$2149 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2248 = ((v$2149 >= 31)?0:((1 << (v$2149 & 31)) & 2147483647));
t$2249 = ((t$2248 - 1) & 2147483647);
t$2251 = (t$2250 & t$2249);
t$2252 = (SmlPrims.w31_to_i32_X(t$2251));
t$2253 = (SmlPrims.chk_ovf_i31(t$2252));
v$650 = [t$2256,t$2253];
var v$651 = v$650[0];
var v$652 = v$650[1];
var fix$2257 = {};
fix$2257.$adj = function(v$624,v$625){lab$adj: while (true) {if (v$625 < 0) {var t$2258 = SmlPrims.chk_ovf_i31(v$624 - 1);
var t$2259 = SmlPrims.chk_ovf_i31(v$625 + v$2161);
var v$624 = t$2258;
var v$625 = t$2259;
continue lab$adj;
} else {return [v$624,v$625];
};
};
};
var adj$615 = fix$2257.$adj;
var v$2150;
if (v$651 == 0) {throw CompilerInitial.exn$Div$47;
} else {v$2150 = (SmlPrims.chk_ovf_i31(SmlPrims.quot(v$657,v$651)));
};
var v$2151;
if (v$651 == 0) {throw CompilerInitial.exn$Div$47;
} else {v$2151 = (v$657 % v$651);
};
var v$644;
var t$2266 = adj$615;
var t$2265 = v$2150;
var t$2264;
var t$2263;
var t$2262;
var t$2261;
var t$2260;
var v$2056 = (SmlPrims.i32_to_w32(v$2151)) & 2147483647;
var v$2057 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2260 = ((v$2057 >= 31)?0:((v$2056 << (v$2057 & 31)) & 2147483647));
t$2261 = (SmlPrims.w31_to_i32_X(t$2260));
t$2262 = (SmlPrims.chk_ovf_i31(t$2261));
t$2263 = (SmlPrims.chk_ovf_i31(t$2262 + v$654));
t$2264 = (SmlPrims.chk_ovf_i31(t$2263 - (SmlPrims.chk_ovf_i31(v$2150 * v$652))));
v$644 = (t$2266(t$2265,t$2264));
var v$645 = v$644[0];
var v$646 = v$644[1];
var v$2152;
if (v$651 == 0) {throw CompilerInitial.exn$Div$47;
} else {v$2152 = (SmlPrims.chk_ovf_i31(SmlPrims.quot(v$646,v$651)));
};
var v$2153;
if (v$651 == 0) {throw CompilerInitial.exn$Div$47;
} else {v$2153 = (v$646 % v$651);
};
var v$638;
var t$2273 = adj$615;
var t$2272 = v$2152;
var t$2271;
var t$2270;
var t$2269;
var t$2268;
var t$2267;
var v$2066 = (SmlPrims.i32_to_w32(v$2153)) & 2147483647;
var v$2067 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2267 = ((v$2067 >= 31)?0:((v$2066 << (v$2067 & 31)) & 2147483647));
t$2268 = (SmlPrims.w31_to_i32_X(t$2267));
t$2269 = (SmlPrims.chk_ovf_i31(t$2268));
t$2270 = (SmlPrims.chk_ovf_i31(t$2269 + v$655));
t$2271 = (SmlPrims.chk_ovf_i31(t$2270 - (SmlPrims.chk_ovf_i31(v$2152 * v$652))));
v$638 = (t$2273(t$2272,t$2271));
var v$639 = v$638[0];
var v$640 = v$638[1];
var t$2277;
var t$2276;
var t$2275;
var t$2274;
var v$2070 = (SmlPrims.i32_to_w32(v$645)) & 2147483647;
var v$2071 = SmlPrims.i32_to_w32(SmlPrims.chk_ovf_i31(SmlPrims.quot(30,2)));
t$2274 = ((v$2071 >= 31)?0:((v$2070 << (v$2071 & 31)) & 2147483647));
t$2275 = (SmlPrims.w31_to_i32_X(t$2274));
t$2276 = (SmlPrims.chk_ovf_i31(t$2275));
t$2277 = (SmlPrims.chk_ovf_i31(t$2276 + v$639));
return [t$2277,v$640];
};
var divmodd$660 = function(v$2162,v$671){switch (v$671) { case 1: {return [v$2162,0];
 break; }default: {var scale$673 = (v$671 == 1073741823)?1:(SmlPrims.div_i31(-1073741824,SmlPrims.chk_ovf_i31(-(SmlPrims.chk_ovf_i31(v$671 + 1))),CompilerInitial.exn$Div$47));
var i$$674 = SmlPrims.chk_ovf_i31(v$671 * scale$673);
var m$$675 = muld$498(v$2162,scale$673);
var fix$2278 = {};
fix$2278.$dmi = function(v$679){if (v$679 == null) {return [null,0];
} else {var v$698 = v$679;
var v$699 = v$698[0];
var v$700 = v$698[1];
var v$695 = fix$2278.$dmi(v$700);
var v$696 = v$695[0];
var v$697 = v$695[1];
var v$692 = divmod2$603([v$697,v$699],i$$674);
var v$693 = v$692[0];
var t$2279;
switch (v$693) { case 0: {t$2279 = ((v$696 == null)?null:[v$693,v$696]);
 break; }default: {t$2279 = [v$693,v$696];
} };
return [t$2279,v$692[1]];
};
};
var dmi$676 = fix$2278.$dmi;
var v$708 = dmi$676(m$$675);
var v$709 = v$708[0];
var v$710 = v$708[1];
if (scale$673 == 0) {throw [basis$0Initial$1.en$Fail$54,"divmodd"];
} else {0;
};
return [v$709,SmlPrims.div_i31(v$710,scale$673,CompilerInitial.exn$Div$47)];
} };
};
var fix$2280 = {};
fix$2280.$divmod = function(v$2163,v$735){if (v$735 == null) {throw CompilerInitial.exn$Div$47;
} else {if (v$2163 == null) {return [null,null];
} else {var v$738 = v$735;
var v$739 = v$738[0];
switch (v$739) { case 0: {var v$746 = v$2163;
var v$747 = v$746[0];
var v$748 = v$746[1];
var v$749 = v$738[1];
var v$743 = fix$2280.$divmod(v$748,v$749);
var v$744 = v$743[0];
var v$745 = v$743[1];
var t$2321 = v$744;
var t$2320;
switch (v$747) { case 0: {t$2320 = ((v$745 == null)?null:[v$747,v$745]);
 break; }default: {t$2320 = [v$747,v$745];
} };
return [t$2321,t$2320];
 break; }default: {if (v$738[1] == null) {var v$758 = divmodd$660(v$2163,v$739);
var v$759 = v$758[0];
var v$760 = v$758[1];
return [v$759,(v$760 == 0)?null:[v$760,null]];
} else {var ln$761;
var fix$2322 = {};
fix$2322.$acc = function(v$1687,v$1688){lab$acc: while (true) {if (v$1687 == null) {return v$1688;
} else {var v$1689 = v$1687;
var v$1690 = v$1689[1];
var t$2323 = v$1690;
var t$2324 = SmlPrims.chk_ovf_i32(v$1688 + 1);
var v$1687 = t$2323;
var v$1688 = t$2324;
continue lab$acc;
};
};
};
var acc$1686 = fix$2322.$acc;
ln$761 = (acc$1686(v$735,0));
var scale$762;
var i$1691;
var v$2079 = SmlPrims.chk_ovf_i32(ln$761 - 1);
var fix$2325 = {};
fix$2325.$h = function(v$1696,v$1697){lab$h: while (true) {if (v$1696 == null) {throw CompilerInitial.exn$Subscript$52;
} else {var v$1698 = v$1696;
var v$1699 = v$1698[0];
var v$1700 = v$1698[1];
if (v$1697 == 0) {return v$1699;
} else {var t$2326 = v$1700;
var t$2327 = SmlPrims.chk_ovf_i32(v$1697 - 1);
var v$1696 = t$2326;
var v$1697 = t$2327;
continue lab$h;
};
};
};
};
var h$1695 = fix$2325.$h;
if (v$2079 < 0) {throw CompilerInitial.exn$Subscript$52;
} else {i$1691 = (h$1695(v$735,v$2079));
};
scale$762 = ((i$1691 == 1073741823)?1:(SmlPrims.div_i31(-1073741824,SmlPrims.chk_ovf_i31(-(SmlPrims.chk_ovf_i31(i$1691 + 1))),CompilerInitial.exn$Div$47)));
var m$$763 = muld$498(v$2163,scale$762);
var n$$764 = muld$498(v$735,scale$762);
var n1$765;
var v$2081 = SmlPrims.chk_ovf_i32(ln$761 - 1);
var fix$2328 = {};
fix$2328.$h = function(v$1705,v$1706){lab$h: while (true) {if (v$1705 == null) {throw CompilerInitial.exn$Subscript$52;
} else {var v$1707 = v$1705;
var v$1708 = v$1707[0];
var v$1709 = v$1707[1];
if (v$1706 == 0) {return v$1708;
} else {var t$2329 = v$1709;
var t$2330 = SmlPrims.chk_ovf_i32(v$1706 - 1);
var v$1705 = t$2329;
var v$1706 = t$2330;
continue lab$h;
};
};
};
};
var h$1704 = fix$2328.$h;
if (v$2081 < 0) {throw CompilerInitial.exn$Subscript$52;
} else {n1$765 = (h$1704(n$$764,v$2081));
};
var fix$2331 = {};
fix$2331.$divl = function(v$769){if (v$769 == null) {return [null,null];
} else {var v$850 = v$769;
var v$851 = v$850[0];
var v$852 = v$850[1];
var v$847 = fix$2331.$divl(v$852);
var v$848 = v$847[0];
var v$849 = v$847[1];
var m$779;
switch (v$851) { case 0: {m$779 = ((v$849 == null)?null:[v$851,v$849]);
 break; }default: {m$779 = [v$851,v$849];
} };
var fix$2332 = {};
fix$2332.$msds = function(v$800,v$2164){lab$msds: while (true) {if (v$800 == null) {return [0,0];
} else {var v$801 = v$800;
var v$802 = v$801[1];
if (v$802 == null) {switch (v$2164) { case 1: {return [0,v$801[0]];
 break; }default: {var v$1713 = v$800;
var v$1714 = v$1713[1];
var t$2333 = v$1714;
var t$2334 = SmlPrims.chk_ovf_i32(v$2164 - 1);
var v$800 = t$2333;
var v$2164 = t$2334;
continue lab$msds;
} };
} else {var v$811 = v$802;
if (v$811[1] == null) {switch (v$2164) { case 1: {return [v$811[0],v$801[0]];
 break; }default: {var v$1718 = v$800;
var v$1719 = v$1718[1];
var t$2335 = v$1719;
var t$2336 = SmlPrims.chk_ovf_i32(v$2164 - 1);
var v$800 = t$2335;
var v$2164 = t$2336;
continue lab$msds;
} };
} else {var v$1723 = v$800;
var v$1724 = v$1723[1];
var t$2337 = v$1724;
var t$2338 = SmlPrims.chk_ovf_i32(v$2164 - 1);
var v$800 = t$2337;
var v$2164 = t$2338;
continue lab$msds;
};
};
};
};
};
var msds$780 = fix$2332.$msds;
var v$844 = msds$780(m$779,ln$761);
var v$845 = v$844[0];
var v$846 = v$844[1];
var tq$819 = (v$845 == n1$765)?1073741823:(divmod2$603([v$845,v$846],n1$765))[0];
var fix$2339 = {};
fix$2339.$try = function(v$836,v$837){try {return [v$836,subt$399(m$779,v$837)];
} catch(v$2341) {return (function(Negative$835){var t$2342 = Negative$835;
if (t$2342[0] == en$Negative$380) {return fix$2339.$try(SmlPrims.chk_ovf_i31(v$836 - 1),subt$399(v$837,n$$764));
} else {throw Negative$835;
};
})(v$2341);
};
};
var try$828 = fix$2339.$try;
var v$841 = try$828(tq$819,muld$498(n$$764,tq$819));
var v$842 = v$841[0];
var t$2340;
switch (v$842) { case 0: {t$2340 = ((v$848 == null)?null:[v$842,v$848]);
 break; }default: {t$2340 = [v$842,v$848];
} };
return [t$2340,v$841[1]];
};
};
var divl$766 = fix$2331.$divl;
var v$860 = divl$766(m$$763);
var v$861 = v$860[0];
var v$862 = v$860[1];
return [v$861,(divmodd$660(v$862,scale$762))[0]];
};
} };
};
};
};
var divmod$712 = fix$2280.$divmod;
var fix$2281 = {};
fix$2281.$cmp = function(v$879,v$2165){if (v$879 == null) {return (v$2165 == null)?2:0;
} else {if (v$2165 == null) {return 1;
} else {var v$895 = v$879;
var v$896 = v$895[0];
var v$897 = v$895[1];
var v$898 = v$2165;
var v$899 = v$898[0];
var v$900 = v$898[1];
var v$886 = fix$2281.$cmp(v$897,v$900);
switch (v$886) { case 2: {return (v$896 == v$899)?2:((v$896 < v$899)?0:1);
 break; }default: {return v$886;
} };
};
};
};
var cmp$863 = fix$2281.$cmp;
var fix$2282 = {};
fix$2282.$natInfToI32 = function(v$949){if (v$949 == null) {return 0;
} else {var v$963 = v$949;
var v$964 = v$963[1];
if (v$964 == null) {return v$963[0];
} else {var v$966 = v$964;
if (v$966[1] == null) {var v$968 = v$963[0];
return SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(-(SmlPrims.chk_ovf_i32((-1073741824) * v$966[0])))) + v$968);
} else {var v$970 = v$963[0];
return SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(-(SmlPrims.chk_ovf_i32((-1073741824) * (fix$2282.$natInfToI32(v$964)))))) + v$970);
};
};
};
};
var natInfToI32$946 = fix$2282.$natInfToI32;
var intInfToI32$983 = function(v$986){var v$995 = v$986[1];
var v$996 = v$995[0];
if (v$996 == null) {return 0;
} else {if (v$995[1]) {try {return SmlPrims.chk_ovf_i32(-(natInfToI32$946(v$996)));
} catch(v$2283) {return (function(v$999){var t$2287;
var v$2091;
var t$2286 = addOne$274;
var t$2284;
var v$1786 = SmlPrims.chk_ovf_i32(-(SmlPrims.chk_ovf_i32((-2147483648) + 1)));
switch (v$1786) { case 0: {t$2284 = null;
 break; }default: {var fix$2285 = {};
fix$2285.$bn = function(v$1788){switch (v$1788) { case 0: {return null;
 break; }default: {var v$2092 = v$1788 >>> 30;
return [SmlPrims.chk_ovf_i31(SmlPrims.chk_ovf_i32(v$1788 & 1073741823)),fix$2285.$bn(v$2092)];
} };
};
var bn$1787 = fix$2285.$bn;
t$2284 = ((v$1786 <= 1073741823)?[SmlPrims.chk_ovf_i31(v$1786),null]:(bn$1787(SmlPrims.i32_to_w32(v$1786))));
} };
v$2091 = (t$2286(t$2284));
var fix$2288 = {};
fix$2288.$eq_list = function(v$2181,v$2182){lab$eq_list: while (true) {if (v$2181 == null) {return (v$2182 == null)?true:false;
} else {if (v$2182 == null) {return false;
} else {var v$2183 = v$2181;
var v$2184 = v$2182;
if (v$2183[0] == v$2184[0]) {var t$2289 = v$2183[1];
var t$2290 = v$2184[1];
var v$2181 = t$2289;
var v$2182 = t$2290;
continue lab$eq_list;
} else {return false;
};
};
};
};
};
var eq_list$2180 = fix$2288.$eq_list;
t$2287 = (eq_list$2180(v$996,v$2091));
if (t$2287) {return -2147483648;
} else {throw CompilerInitial.exn$Overflow$50;
};
})(v$2283);
};
} else {return natInfToI32$946(v$996);
};
};
};
var i32ToIntInf$1009 = function(v$1012){switch (v$1012) { case 0: {return [0,[null,false]];
 break; }default: {if (v$1012 < 0) {if (v$1012 == (-2147483648)) {var t$2295;
var t$2294;
var t$2293 = addOne$274;
var t$2291;
var v$1800 = SmlPrims.chk_ovf_i32(-(SmlPrims.chk_ovf_i32((-2147483648) + 1)));
switch (v$1800) { case 0: {t$2291 = null;
 break; }default: {var fix$2292 = {};
fix$2292.$bn = function(v$1802){switch (v$1802) { case 0: {return null;
 break; }default: {var v$2098 = v$1802 >>> 30;
return [SmlPrims.chk_ovf_i31(SmlPrims.chk_ovf_i32(v$1802 & 1073741823)),fix$2292.$bn(v$2098)];
} };
};
var bn$1801 = fix$2292.$bn;
t$2291 = ((v$1800 <= 1073741823)?[SmlPrims.chk_ovf_i31(v$1800),null]:(bn$1801(SmlPrims.i32_to_w32(v$1800))));
} };
t$2294 = (t$2293(t$2291));
t$2295 = [t$2294,true];
return [0,t$2295];
} else {var t$2298;
var t$2296;
var v$1809 = SmlPrims.chk_ovf_i32(-(v$1012));
switch (v$1809) { case 0: {t$2296 = null;
 break; }default: {var fix$2297 = {};
fix$2297.$bn = function(v$1811){switch (v$1811) { case 0: {return null;
 break; }default: {var v$2104 = v$1811 >>> 30;
return [SmlPrims.chk_ovf_i31(SmlPrims.chk_ovf_i32(v$1811 & 1073741823)),fix$2297.$bn(v$2104)];
} };
};
var bn$1810 = fix$2297.$bn;
t$2296 = ((v$1809 <= 1073741823)?[SmlPrims.chk_ovf_i31(v$1809),null]:(bn$1810(SmlPrims.i32_to_w32(v$1809))));
} };
t$2298 = [t$2296,true];
return [0,t$2298];
};
} else {var t$2301;
var t$2299;
switch (v$1012) { case 0: {t$2299 = null;
 break; }default: {var fix$2300 = {};
fix$2300.$bn = function(v$1821){switch (v$1821) { case 0: {return null;
 break; }default: {var v$2110 = v$1821 >>> 30;
return [SmlPrims.chk_ovf_i31(SmlPrims.chk_ovf_i32(v$1821 & 1073741823)),fix$2300.$bn(v$2110)];
} };
};
var bn$1820 = fix$2300.$bn;
t$2299 = ((v$1012 <= 1073741823)?[SmlPrims.chk_ovf_i31(v$1012),null]:(bn$1820(SmlPrims.i32_to_w32(v$1012))));
} };
t$2301 = [t$2299,false];
return [0,t$2301];
};
} };
};
basis$0IntInfRep$1.toInt31$1031 = function(x$1034){return SmlPrims.chk_ovf_i31(intInfToI32$983(x$1034));
};
basis$0IntInfRep$1.toInt32$1035 = function(x$1038){return intInfToI32$983(x$1038);
};
basis$0IntInfRep$1.toInt$1039 = function(x$1042){return intInfToI32$983(x$1042);
};
basis$0IntInfRep$1.fromInt$1043 = function(x$1046){return i32ToIntInf$1009(x$1046);
};
basis$0IntInfRep$1.fromInt32$1047 = function(x$1050){return i32ToIntInf$1009(x$1050);
};
basis$0IntInfRep$1.fromInt31$1051 = function(x$1054){return i32ToIntInf$1009(x$1054);
};
var IntInfPlus$1083 = function(v$1107,v$2167){var v$1108 = v$1107[1];
var v$1109 = v$1108[0];
if (v$1109 == null) {return v$2167;
} else {var v$1112 = v$2167[1];
var v$1113 = v$1112[0];
if (v$1113 == null) {return v$1107;
} else {if (v$1108[1]) {if (v$1112[1]) {var v$1852 = v$1107[1];
var v$1853 = v$1852[1];
var v$1854 = v$1852[0];
var v$1856 = v$2167[1];
var v$1857 = v$1856[0];
return [0,[add$294(v$1854,v$1857),v$1853]];
} else {var t$2303;
if (v$1109 == null) {t$2303 = [v$1113,false];
} else {if (v$1113 == null) {t$2303 = [v$1109,true];
} else {try {t$2303 = [subt$399(v$1113,v$1109),false];
} catch(v$2302) {t$2303 = ((function(BN$Negative$1849){var t$2304 = BN$Negative$1849;
if (t$2304[0] == en$Negative$380) {return [subt$399(v$1109,v$1113),true];
} else {throw BN$Negative$1849;
};
})(v$2302));
};
};
};
return [0,t$2303];
};
} else {if (v$1112[1]) {var t$2306;
if (v$1113 == null) {t$2306 = [v$1109,false];
} else {if (v$1109 == null) {t$2306 = [v$1113,true];
} else {try {t$2306 = [subt$399(v$1109,v$1113),false];
} catch(v$2305) {t$2306 = ((function(BN$Negative$1837){var t$2307 = BN$Negative$1837;
if (t$2307[0] == en$Negative$380) {return [subt$399(v$1113,v$1109),true];
} else {throw BN$Negative$1837;
};
})(v$2305));
};
};
};
return [0,t$2306];
} else {var v$1840 = v$1107[1];
var v$1841 = v$1840[1];
var v$1842 = v$1840[0];
var v$1844 = v$2167[1];
var v$1845 = v$1844[0];
return [0,[add$294(v$1842,v$1845),v$1841]];
};
};
};
};
};
basis$0IntInfRep$1.fromWord32$1156 = function(w$1159){if (w$1159 <= 2147483647) {var x$1858 = SmlPrims.chk_ovf_i32(w$1159);
return i32ToIntInf$1009(x$1858);
} else {var w2$1164;
var x$1860 = SmlPrims.chk_ovf_i32(SmlPrims.div_w32(w$1159,3,CompilerInitial.exn$Div$47));
w2$1164 = (i32ToIntInf$1009(x$1860));
var rest$1165;
var x$1862 = SmlPrims.chk_ovf_i32(SmlPrims.mod_w32(w$1159,3,CompilerInitial.exn$Div$47));
rest$1165 = (i32ToIntInf$1009(x$1862));
return IntInfPlus$1083(IntInfPlus$1083(IntInfPlus$1083(w2$1164,w2$1164),w2$1164),rest$1165);
};
};
basis$0IntInfRep$1.fromWord32X$1167 = function(w$1170){var x$1864 = SmlPrims.w32_to_i32_X(w$1170);
return i32ToIntInf$1009(x$1864);
};
basis$0IntInfRep$1.fromWord31X$1171 = function(w$1174){var w$1866 = SmlPrims.w31_to_w32_X(w$1174);
var x$1867 = SmlPrims.w32_to_i32_X(w$1866);
return i32ToIntInf$1009(x$1867);
};
basis$0IntInfRep$1.fromWordX$1175 = function(w$1178){var w$1869 = w$1178;
var x$1870 = SmlPrims.w32_to_i32_X(w$1869);
return i32ToIntInf$1009(x$1870);
};
basis$0IntInfRep$1.toWord32$1179 = function(x$1182){return SmlPrims.i32_to_w32(intInfToI32$983(x$1182));
};
basis$0IntInfRep$1.fromWord$1183 = function(w$1186){var w$1874 = w$1186;
if (w$1874 <= 2147483647) {var x$1875 = SmlPrims.chk_ovf_i32(w$1874);
return i32ToIntInf$1009(x$1875);
} else {var w2$1876;
var x$1877 = SmlPrims.chk_ovf_i32(SmlPrims.div_w32(w$1874,3,CompilerInitial.exn$Div$47));
w2$1876 = (i32ToIntInf$1009(x$1877));
var rest$1878;
var x$1879 = SmlPrims.chk_ovf_i32(SmlPrims.mod_w32(w$1874,3,CompilerInitial.exn$Div$47));
rest$1878 = (i32ToIntInf$1009(x$1879));
return IntInfPlus$1083(IntInfPlus$1083(IntInfPlus$1083(w2$1876,w2$1876),w2$1876),rest$1878);
};
};
basis$0IntInfRep$1.toWord$1187 = function(x$1190){return SmlPrims.i32_to_w32(intInfToI32$983(x$1190));
};
basis$0IntInfRep$1.fromWord8$1191 = function(w8$1194){var w$1884 = w8$1194;
if (w$1884 <= 2147483647) {var x$1885 = SmlPrims.chk_ovf_i32(w$1884);
return i32ToIntInf$1009(x$1885);
} else {var w2$1886;
var x$1887 = SmlPrims.chk_ovf_i32(SmlPrims.div_w32(w$1884,3,CompilerInitial.exn$Div$47));
w2$1886 = (i32ToIntInf$1009(x$1887));
var rest$1888;
var x$1889 = SmlPrims.chk_ovf_i32(SmlPrims.mod_w32(w$1884,3,CompilerInitial.exn$Div$47));
rest$1888 = (i32ToIntInf$1009(x$1889));
return IntInfPlus$1083(IntInfPlus$1083(IntInfPlus$1083(w2$1886,w2$1886),w2$1886),rest$1888);
};
};
basis$0IntInfRep$1.fromWord8X$1195 = function(w$1198){if (w$1198 < 128) {var w$1892 = w$1198;
if (w$1892 <= 2147483647) {var x$1893 = SmlPrims.chk_ovf_i32(w$1892);
return i32ToIntInf$1009(x$1893);
} else {var w2$1894;
var x$1895 = SmlPrims.chk_ovf_i32(SmlPrims.div_w32(w$1892,3,CompilerInitial.exn$Div$47));
w2$1894 = (i32ToIntInf$1009(x$1895));
var rest$1896;
var x$1897 = SmlPrims.chk_ovf_i32(SmlPrims.mod_w32(w$1892,3,CompilerInitial.exn$Div$47));
rest$1896 = (i32ToIntInf$1009(x$1897));
return IntInfPlus$1083(IntInfPlus$1083(IntInfPlus$1083(w2$1894,w2$1894),w2$1894),rest$1896);
};
} else {var v$1898;
var w8$1901 = 255 & (((255 & ((255 - w$1198) & 4294967295)) + 1) & 4294967295);
var w$1902 = w8$1901;
if (w$1902 <= 2147483647) {var x$1903 = SmlPrims.chk_ovf_i32(w$1902);
v$1898 = (i32ToIntInf$1009(x$1903));
} else {var w2$1904;
var x$1905 = SmlPrims.chk_ovf_i32(SmlPrims.div_w32(w$1902,3,CompilerInitial.exn$Div$47));
w2$1904 = (i32ToIntInf$1009(x$1905));
var rest$1906;
var x$1907 = SmlPrims.chk_ovf_i32(SmlPrims.mod_w32(w$1902,3,CompilerInitial.exn$Div$47));
rest$1906 = (i32ToIntInf$1009(x$1907));
v$1898 = (IntInfPlus$1083(IntInfPlus$1083(IntInfPlus$1083(w2$1904,w2$1904),w2$1904),rest$1906));
};
var v$1899 = v$1898[1];
var v$1900 = v$1899[0];
return (v$1900 == null)?v$1898:(v$1899[1]?[0,[v$1900,false]]:[0,[v$1900,true]]);
};
};
basis$0IntInfRep$1.toWord8$1203 = function(x$1206){return 255 & (SmlPrims.i32_to_w32(intInfToI32$983(x$1206)));
};
basis$0IntInfRep$1.fromWord31$1207 = function(w31$1210){var w$1913 = w31$1210;
if (w$1913 <= 2147483647) {var x$1914 = SmlPrims.chk_ovf_i32(w$1913);
return i32ToIntInf$1009(x$1914);
} else {var w2$1915;
var x$1916 = SmlPrims.chk_ovf_i32(SmlPrims.div_w32(w$1913,3,CompilerInitial.exn$Div$47));
w2$1915 = (i32ToIntInf$1009(x$1916));
var rest$1917;
var x$1918 = SmlPrims.chk_ovf_i32(SmlPrims.mod_w32(w$1913,3,CompilerInitial.exn$Div$47));
rest$1917 = (i32ToIntInf$1009(x$1918));
return IntInfPlus$1083(IntInfPlus$1083(IntInfPlus$1083(w2$1915,w2$1915),w2$1915),rest$1917);
};
};
basis$0IntInfRep$1.toWord31$1211 = function(x$1214){return (SmlPrims.i32_to_w32(intInfToI32$983(x$1214))) & 2147483647;
};
basis$0IntInfRep$1.s$p$1215 = function(v$1923){var v$1924 = v$1923[1];
var v$1925 = v$1924[0];
return (v$1925 == null)?v$1923:(v$1924[1]?[0,[v$1925,false]]:[0,[v$1925,true]]);
};
basis$0IntInfRep$1.s$t$1216 = function(v$2168,v$1237){var v$1238 = v$1237[1];
var v$1239 = v$1238[0];
if (v$1239 == null) {return [0,[null,false]];
} else {var v$1241 = v$2168[1];
var v$1242 = v$1241[0];
if (v$1242 == null) {return [0,[null,false]];
} else {if (v$1241[1]) {if (v$1238[1]) {var v$1937 = v$2168[1];
var v$1938 = v$1937[0];
var v$1940 = v$1237[1];
var v$1941 = v$1940[0];
return [0,[mult$547(v$1938,v$1941),false]];
} else {return [0,[mult$547(v$1242,v$1239),true]];
};
} else {if (v$1238[1]) {return [0,[mult$547(v$1242,v$1239),true]];
} else {var v$1930 = v$2168[1];
var v$1931 = v$1930[0];
var v$1933 = v$1237[1];
var v$1934 = v$1933[0];
return [0,[mult$547(v$1931,v$1934),false]];
};
};
};
};
};
basis$0IntInfRep$1.s$f$1259 = function(v$2169){return IntInfPlus$1083(v$2169[0],v$2169[1]);
};
basis$0IntInfRep$1.s$g$1260 = function(v$2170,v$1285){var v$1286 = v$1285[1];
var v$1287 = v$1286[0];
if (v$1287 == null) {return v$2170;
} else {var v$1290 = v$2170[1];
var v$1291 = v$1290[0];
if (v$1291 == null) {var v$1294 = v$1286[1];
return [0,[v$1287,v$1294?false:true]];
} else {if (v$1290[1]) {if (v$1286[1]) {var t$2309;
if (v$1291 == null) {t$2309 = [v$1287,false];
} else {if (v$1287 == null) {t$2309 = [v$1291,true];
} else {try {t$2309 = [subt$399(v$1287,v$1291),false];
} catch(v$2308) {t$2309 = ((function(BN$Negative$1958){var t$2310 = BN$Negative$1958;
if (t$2310[0] == en$Negative$380) {return [subt$399(v$1291,v$1287),true];
} else {throw BN$Negative$1958;
};
})(v$2308));
};
};
};
return [0,t$2309];
} else {var v$1961 = v$2170[1];
var v$1962 = v$1961[1];
var v$1963 = v$1961[0];
var v$1965 = v$1285[1];
var v$1966 = v$1965[0];
return [0,[add$294(v$1963,v$1966),v$1962]];
};
} else {if (v$1286[1]) {var v$1949 = v$2170[1];
var v$1950 = v$1949[1];
var v$1951 = v$1949[0];
var v$1953 = v$1285[1];
var v$1954 = v$1953[0];
return [0,[add$294(v$1951,v$1954),v$1950]];
} else {var t$2312;
if (v$1287 == null) {t$2312 = [v$1291,false];
} else {if (v$1291 == null) {t$2312 = [v$1287,true];
} else {try {t$2312 = [subt$399(v$1291,v$1287),false];
} catch(v$2311) {t$2312 = ((function(BN$Negative$1946){var t$2313 = BN$Negative$1946;
if (t$2313[0] == en$Negative$380) {return [subt$399(v$1287,v$1291),true];
} else {throw BN$Negative$1946;
};
})(v$2311));
};
};
};
return [0,t$2312];
};
};
};
};
};
var divmod$1331 = function(v$1354,v$2171){var v$1355 = v$1354[1];
if (v$1355[1]) {var v$1378 = v$2171[1];
if (v$1378[1]) {var v$1394 = v$1355[0];
var v$1395 = v$1378[0];
var v$1391 = divmod$712(v$1394,v$1395);
var v$1392 = v$1391[0];
var v$1393 = v$1391[1];
return [[0,[v$1392,false]],(v$1393 == null)?[0,[null,false]]:[0,[v$1393,true]]];
} else {var v$1386 = v$1355[0];
var v$1387 = v$1378[0];
var v$1383 = divmod$712(subtOne$355(v$1386),v$1387);
var v$1384 = v$1383[0];
var v$1385 = v$1383[1];
return [[0,[addOne$274(v$1384),true]],[0,[subtOne$355(subt$399(v$1387,v$1385)),false]]];
};
} else {var v$1358 = v$2171[1];
if (v$1358[1]) {var v$1368 = v$1355[0];
if (v$1368 == null) {return [[0,[null,false]],[0,[null,false]]];
} else {var v$1376 = v$1358[0];
var v$1373 = divmod$712(subtOne$355(v$1368),v$1376);
var v$1374 = v$1373[0];
var v$1375 = v$1373[1];
var t$2315 = [0,[addOne$274(v$1374),true]];
var t$2314;
var v$1973 = subtOne$355(subt$399(v$1376,v$1375));
t$2314 = ((v$1973 == null)?[0,[null,false]]:[0,[v$1973,true]]);
return [t$2315,t$2314];
};
} else {var v$1366 = v$1355[0];
var v$1367 = v$1358[0];
var v$1363 = divmod$712(v$1366,v$1367);
return [[0,[v$1363[0],false]],[0,[v$1363[1],false]]];
};
};
};
basis$0IntInfRep$1.div$1396 = function(arg$1399){return (divmod$1331(arg$1399[0],arg$1399[1]))[0];
};
basis$0IntInfRep$1.mod$1404 = function(arg$1407){return (divmod$1331(arg$1407[0],arg$1407[1]))[1];
};
basis$0IntInfRep$1.abs$1412 = function(v$1415){var v$1422 = v$1415[1];
return v$1422[1]?[0,[v$1422[0],false]]:v$1415;
};
basis$0IntInfRep$1.s$j$1456 = function(v$2172,v$2173){var t$2316;
var v$1980 = v$2172[1];
if (v$1980[1]) {var v$1982 = v$2173[1];
if (v$1982[1]) {var v$1983 = v$1980[0];
var v$1984 = v$1982[0];
t$2316 = (cmp$863(v$1984,v$1983));
} else {t$2316 = 0;
};
} else {var v$1986 = v$2173[1];
if (v$1986[1]) {t$2316 = 1;
} else {var v$1987 = v$1980[0];
var v$1988 = v$1986[0];
t$2316 = (cmp$863(v$1987,v$1988));
};
};
switch (t$2316) { case 0: {return true;
 break; }default: {return false;
} };
};
basis$0IntInfRep$1.s$l$1464 = function(v$2174,v$2175){var t$2317;
var v$1991 = v$2174[1];
if (v$1991[1]) {var v$1993 = v$2175[1];
if (v$1993[1]) {var v$1994 = v$1991[0];
var v$1995 = v$1993[0];
t$2317 = (cmp$863(v$1995,v$1994));
} else {t$2317 = 0;
};
} else {var v$1997 = v$2175[1];
if (v$1997[1]) {t$2317 = 1;
} else {var v$1998 = v$1991[0];
var v$1999 = v$1997[0];
t$2317 = (cmp$863(v$1998,v$1999));
};
};
switch (t$2317) { case 1: {return true;
 break; }default: {return false;
} };
};
basis$0IntInfRep$1.s$jk$1472 = function(v$2176,v$2177){var t$2318;
var v$2002 = v$2176[1];
if (v$2002[1]) {var v$2004 = v$2177[1];
if (v$2004[1]) {var v$2005 = v$2002[0];
var v$2006 = v$2004[0];
t$2318 = (cmp$863(v$2006,v$2005));
} else {t$2318 = 0;
};
} else {var v$2008 = v$2177[1];
if (v$2008[1]) {t$2318 = 1;
} else {var v$2009 = v$2002[0];
var v$2010 = v$2008[0];
t$2318 = (cmp$863(v$2009,v$2010));
};
};
switch (t$2318) { case 1: {return false;
 break; }default: {return true;
} };
};
basis$0IntInfRep$1.s$lk$1480 = function(v$2178,v$2179){var t$2319;
var v$2013 = v$2178[1];
if (v$2013[1]) {var v$2015 = v$2179[1];
if (v$2015[1]) {var v$2016 = v$2013[0];
var v$2017 = v$2015[0];
t$2319 = (cmp$863(v$2017,v$2016));
} else {t$2319 = 0;
};
} else {var v$2019 = v$2179[1];
if (v$2019[1]) {t$2319 = 1;
} else {var v$2020 = v$2013[0];
var v$2021 = v$2019[0];
t$2319 = (cmp$863(v$2020,v$2021));
};
};
switch (t$2319) { case 0: {return false;
 break; }default: {return true;
} };
};
return 0;
})();