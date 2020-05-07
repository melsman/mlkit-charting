if ((typeof(prettyprint$0prettyprint$1)) == "undefined") {prettyprint$0prettyprint$1 = {};
};
(function(){prettyprint$0prettyprint$1.decorate$50 = function(v$53,v$56){if (v$53) {var attrs$419 = [["class","positive-number"],null];
var newelem$421 = document.createElement("span");
var fix$766 = {};
fix$766.$app = function(v$423){lab$app: while (true) {if (v$423 == null) {return 0;
} else {var v$424 = v$423;
var v$425 = v$424[0];
var v$426 = v$424[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$421,v$425[0],v$425[1]);
var t$767 = v$426;
var v$423 = t$767;
continue lab$app;
};
};
};
var app$422 = fix$766.$app;
app$422(attrs$419);
(function(e,a){return e.appendChild(a);})(newelem$421,v$56);
return newelem$421;
} else {var attrs$428 = [["class","negative-number"],null];
var newelem$430 = document.createElement("span");
var fix$768 = {};
fix$768.$app = function(v$432){lab$app: while (true) {if (v$432 == null) {return 0;
} else {var v$433 = v$432;
var v$434 = v$433[0];
var v$435 = v$433[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$430,v$434[0],v$434[1]);
var t$769 = v$435;
var v$432 = t$769;
continue lab$app;
};
};
};
var app$431 = fix$768.$app;
app$431(attrs$428);
(function(e,a){return e.appendChild(a);})(newelem$430,v$56);
return newelem$430;
};
};
prettyprint$0prettyprint$1.decorateStr$66 = function(v$69,v$72){return v$69?(("<span class='positive-number'>" + v$72) + "</span>"):(("<span class='negative-number'>" + v$72) + "</span>");
};
var fix$770 = {};
fix$770.$mkThousandSeps = function(whole$85){return (whole$85.length > 3)?(((fix$770.$mkThousandSeps(basis$0String$1.substring$165(whole$85,0,SmlPrims.chk_ovf_i32(whole$85.length - 3)))) + ",") + (basis$0String$1.extract$112(whole$85,SmlPrims.chk_ovf_i32(whole$85.length - 3),[1]))):whole$85;
};
prettyprint$0prettyprint$1.mkThousandSeps$82 = fix$770.$mkThousandSeps;
prettyprint$0prettyprint$1.real_to_string_with_sep$90 = function(n$93,r$96){var ppPos$97 = function(r$100){var s$101 = (basis$0Real$1.fmt$160([2,[0,n$93]]))(r$100);
var v$111 = basis$0String$1.tokens$220(function(c$124){return c$124 == 46;
},s$101);
if (v$111 == null) {return s$101;
} else {var v$113 = v$111;
var v$114 = v$113[1];
if (v$114 == null) {var v$121 = v$113[0];
return prettyprint$0prettyprint$1.mkThousandSeps$82(v$121);
} else {var v$116 = v$114;
if (v$116[1] == null) {var v$118 = v$113[0];
var v$119 = v$116[0];
return ((prettyprint$0prettyprint$1.mkThousandSeps$82(v$118)) + ".") + v$119;
} else {return s$101;
};
};
};
};
if (r$96 < 0.0) {return "-" + (ppPos$97(-(r$96)));
} else {return ppPos$97(r$96);
};
};
prettyprint$0prettyprint$1.ppNumberRealStr0$129 = function(n$132,r$135){var v$450 = r$135 >= 0.0;
var v$451 = prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$132,r$135);
return v$450?(("<span class='positive-number'>" + v$451) + "</span>"):(("<span class='negative-number'>" + v$451) + "</span>");
};
prettyprint$0prettyprint$1.ppNumberRealStr$136 = function(r$139){var v$742 = r$139 >= 0.0;
var v$743 = prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,r$139);
return v$742?(("<span class='positive-number'>" + v$743) + "</span>"):(("<span class='negative-number'>" + v$743) + "</span>");
};
prettyprint$0prettyprint$1.ppNumberReal0$140 = function(n$143,r$146){return prettyprint$0prettyprint$1.decorate$50(r$146 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$143,r$146)));
};
prettyprint$0prettyprint$1.ppNumberReal$147 = function(r$150){return prettyprint$0prettyprint$1.decorate$50(r$150 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,r$150)));
};
prettyprint$0prettyprint$1.ppNumber0$151 = function(n$154,s$157){var v$162 = basis$0Real$1.fromString$493(s$157);
switch (v$162[0]) { case 0: {var v$164 = v$162[1];
return prettyprint$0prettyprint$1.decorate$50(v$164 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$154,v$164)));
 break; }default: {return document.createTextNode(s$157);
} };
};
prettyprint$0prettyprint$1.ppBillions0$165 = function(n$168,r$171){return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$168,r$171 / 1000000000.0)) + "B";
};
prettyprint$0prettyprint$1.ppBillions$172 = function(r$175){return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,r$175 / 1000000000.0)) + "B";
};
prettyprint$0prettyprint$1.ppMillions0$176 = function(n$179,r$182){return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$179,r$182 / 1000000.0)) + "M";
};
prettyprint$0prettyprint$1.ppMillions$183 = function(r$186){return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,r$186 / 1000000.0)) + "M";
};
prettyprint$0prettyprint$1.ppThousands0$187 = function(n$190,r$193){return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,r$193 / 1000.0)) + "k";
};
prettyprint$0prettyprint$1.ppThousands$194 = function(r$197){return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,r$197 / 1000.0)) + "k";
};
prettyprint$0prettyprint$1.ppNumber0$$198 = function(n$201,r$204){return prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$201,r$204);
};
prettyprint$0prettyprint$1.ppNumber$$205 = function(r$208){return prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,r$208);
};
prettyprint$0prettyprint$1.ppNumber$209 = function(s$212){var v$485 = basis$0Real$1.fromString$493(s$212);
switch (v$485[0]) { case 0: {var v$486 = v$485[1];
return prettyprint$0prettyprint$1.decorate$50(v$486 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,v$486)));
 break; }default: {return document.createTextNode(s$212);
} };
};
prettyprint$0prettyprint$1.ppNumberStr0$$213 = function(n$216,s$219){var v$224 = basis$0Real$1.fromString$493(s$219);
switch (v$224[0]) { case 0: {var v$226 = v$224[1];
return prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$216,v$226);
 break; }default: {return s$219;
} };
};
prettyprint$0prettyprint$1.ppNumberStr$$227 = function(s$230){var v$493 = basis$0Real$1.fromString$493(s$230);
switch (v$493[0]) { case 0: {var v$494 = v$493[1];
return prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,v$494);
 break; }default: {return s$230;
} };
};
prettyprint$0prettyprint$1.ppSmart0$231 = function(n$234,r$237){if (r$237 > 999999999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$234,r$237 / 1000000000.0)) + "B";
} else {if (r$237 > 999999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$234,r$237 / 1000000.0)) + "M";
} else {if (r$237 > 999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,r$237 / 1000.0)) + "k";
} else {return prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$234,r$237);
};
};
};
};
prettyprint$0prettyprint$1.ppSmart$250 = function(r$253){return prettyprint$0prettyprint$1.ppSmart0$231(2,r$253);
};
prettyprint$0prettyprint$1.ppSmartStr$254 = function(s$257){var v$262 = basis$0Real$1.fromString$493(s$257);
switch (v$262[0]) { case 0: {var v$264 = v$262[1];
return prettyprint$0prettyprint$1.ppSmart0$231(2,v$264);
 break; }default: {return s$257;
} };
};
prettyprint$0prettyprint$1.ppPercent0$$265 = function(n$268,r$271){return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$268,r$271 * 100.0)) + "%";
};
prettyprint$0prettyprint$1.ppPercent$$272 = function(r$275){return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,r$275 * 100.0)) + "%";
};
prettyprint$0prettyprint$1.ppPercent$276 = function(r$279){return prettyprint$0prettyprint$1.decorate$50(r$279 >= 0.0,document.createTextNode((prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,r$279 * 100.0)) + "%"));
};
prettyprint$0prettyprint$1.ppPercentStr$$280 = function(s$283){var v$288 = basis$0Real$1.fromString$493(s$283);
switch (v$288[0]) { case 0: {var v$290 = v$288[1];
return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,v$290 * 100.0)) + "%";
 break; }default: {return s$283;
} };
};
prettyprint$0prettyprint$1.ppPercentStr$291 = function(s$294){var v$299 = basis$0Real$1.fromString$493(s$294);
switch (v$299[0]) { case 0: {var v$301 = v$299[1];
return prettyprint$0prettyprint$1.decorate$50(v$301 >= 0.0,document.createTextNode((prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,v$301 * 100.0)) + "%"));
 break; }default: {return document.createTextNode(s$294);
} };
};
prettyprint$0prettyprint$1.ppNumberOrInt$302 = function(s$305){var t$771;
var stop$531 = s$305.length;
var fix$772 = {};
fix$772.$lr = function(j$533){lab$lr: while (true) {if (j$533 < stop$531) {if ((s$305.charCodeAt(j$533)) == 46) {return true;
} else {var t$773 = SmlPrims.chk_ovf_i32(j$533 + 1);
var j$533 = t$773;
continue lab$lr;
};
} else {return false;
};
};
};
var lr$532 = fix$772.$lr;
t$771 = (lr$532(0));
if (t$771) {var v$537 = basis$0Real$1.fromString$493(s$305);
switch (v$537[0]) { case 0: {var v$538 = v$537[1];
return prettyprint$0prettyprint$1.decorate$50(v$538 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,v$538)));
 break; }default: {return document.createTextNode(s$305);
} };
} else {var v$314 = basis$0IntInf$1.fromString$2409(s$305);
switch (v$314[0]) { case 0: {var v$320 = v$314[1];
if (basis$0IntInfRep$1.s$lk$1476([v$320,[0,[null,false]]])) {return prettyprint$0prettyprint$1.decorate$50(basis$0IntInfRep$1.s$lk$1476([v$320,[0,[null,false]]]),document.createTextNode(prettyprint$0prettyprint$1.mkThousandSeps$82(s$305)));
} else {return prettyprint$0prettyprint$1.decorate$50(basis$0IntInfRep$1.s$lk$1476([v$320,[0,[null,false]]]),document.createTextNode("-" + (prettyprint$0prettyprint$1.mkThousandSeps$82(basis$0IntInf$1.toString$2347(basis$0IntInfRep$1.s$p$1211(v$320))))));
};
 break; }default: {var v$545 = basis$0Real$1.fromString$493(s$305);
switch (v$545[0]) { case 0: {var v$546 = v$545[1];
return prettyprint$0prettyprint$1.decorate$50(v$546 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,v$546)));
 break; }default: {return document.createTextNode(s$305);
} };
} };
};
};
prettyprint$0prettyprint$1.ppNumberStr0$324 = function(n$327,s$330){var v$335 = basis$0Real$1.fromString$493(s$330);
switch (v$335[0]) { case 0: {var v$337 = v$335[1];
var v$747 = v$337 >= 0.0;
var v$748 = prettyprint$0prettyprint$1.real_to_string_with_sep$90(n$327,v$337);
return v$747?(("<span class='positive-number'>" + v$748) + "</span>"):(("<span class='negative-number'>" + v$748) + "</span>");
 break; }default: {return s$330;
} };
};
prettyprint$0prettyprint$1.ppNumberStr$338 = function(s$341){var v$549 = basis$0Real$1.fromString$493(s$341);
switch (v$549[0]) { case 0: {var v$550 = v$549[1];
var v$751 = v$550 >= 0.0;
var v$752 = prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,v$550);
return v$751?(("<span class='positive-number'>" + v$752) + "</span>"):(("<span class='negative-number'>" + v$752) + "</span>");
 break; }default: {return s$341;
} };
};
prettyprint$0prettyprint$1.ppNumberOrIntStr$342 = function(s$345){var t$774;
var stop$553 = s$345.length;
var fix$775 = {};
fix$775.$lr = function(j$555){lab$lr: while (true) {if (j$555 < stop$553) {if ((s$345.charCodeAt(j$555)) == 46) {return true;
} else {var t$776 = SmlPrims.chk_ovf_i32(j$555 + 1);
var j$555 = t$776;
continue lab$lr;
};
} else {return false;
};
};
};
var lr$554 = fix$775.$lr;
t$774 = (lr$554(0));
if (t$774) {var v$559 = basis$0Real$1.fromString$493(s$345);
switch (v$559[0]) { case 0: {var v$560 = v$559[1];
var v$756 = v$560 >= 0.0;
var v$757 = prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,v$560);
return v$756?(("<span class='positive-number'>" + v$757) + "</span>"):(("<span class='negative-number'>" + v$757) + "</span>");
 break; }default: {return s$345;
} };
} else {var v$354 = basis$0IntInf$1.fromString$2409(s$345);
switch (v$354[0]) { case 0: {var v$360 = v$354[1];
if (basis$0IntInfRep$1.s$lk$1476([v$360,[0,[null,false]]])) {var v$561 = basis$0IntInfRep$1.s$lk$1476([v$360,[0,[null,false]]]);
var v$562 = prettyprint$0prettyprint$1.mkThousandSeps$82(s$345);
return v$561?(("<span class='positive-number'>" + v$562) + "</span>"):(("<span class='negative-number'>" + v$562) + "</span>");
} else {var v$570 = basis$0IntInfRep$1.s$lk$1476([v$360,[0,[null,false]]]);
var v$571 = "-" + (prettyprint$0prettyprint$1.mkThousandSeps$82(basis$0IntInf$1.toString$2347(basis$0IntInfRep$1.s$p$1211(v$360))));
return v$570?(("<span class='positive-number'>" + v$571) + "</span>"):(("<span class='negative-number'>" + v$571) + "</span>");
};
 break; }default: {var v$583 = basis$0Real$1.fromString$493(s$345);
switch (v$583[0]) { case 0: {var v$584 = v$583[1];
var v$760 = v$584 >= 0.0;
var v$761 = prettyprint$0prettyprint$1.real_to_string_with_sep$90(2,v$584);
return v$760?(("<span class='positive-number'>" + v$761) + "</span>"):(("<span class='negative-number'>" + v$761) + "</span>");
 break; }default: {return s$345;
} };
} };
};
};
prettyprint$0prettyprint$1.ppRatio$364 = function(r$367){return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(0,r$367)) + ":1";
};
prettyprint$0prettyprint$1.labelFormatter$368 = function(f$371){return (function(f){return function() { return f(this.name); };})(f$371);
};
prettyprint$0prettyprint$1.pointFormatter$372 = function(f$375){return (function(f){return function() { return f(this); };})(f$375);
};
prettyprint$0prettyprint$1.percentFormatter$376 = function(decimals$379){return (function(f){return function() { return f(this); };})(function(this$762){var value$763 = (function(fp,s){return fp[s];})(this$762,"value");
return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(decimals$379,value$763 * 100.0)) + "%";
});
};
prettyprint$0prettyprint$1.ratioFormatter$384 = function(decimals$387){return (function(f){return function() { return f(this); };})(function(this$764){var value$765 = (function(fp,s){return fp[s];})(this$764,"value");
return (prettyprint$0prettyprint$1.real_to_string_with_sep$90(decimals$387,value$765)) + ":1";
});
};
return 0;
})();