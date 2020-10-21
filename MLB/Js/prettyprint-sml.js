if ((typeof(prettyprint$0prettyprint$1)) == "undefined") {prettyprint$0prettyprint$1 = {};
};
(function(){prettyprint$0prettyprint$1.decorate$54 = function(v$57,v$60){if (v$57) {var attrs$423 = [["class","positive-number"],null];
var newelem$425 = document.createElement("span");
var fix$830 = {};
fix$830.$app = function(v$427){lab$app: while (true) {if (v$427 == null) {return 0;
} else {var v$428 = v$427;
var v$429 = v$428[0];
var v$430 = v$428[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$425,v$429[0],v$429[1]);
var t$831 = v$430;
var v$427 = t$831;
continue lab$app;
};
};
};
var app$426 = fix$830.$app;
app$426(attrs$423);
(function(e,a){return e.appendChild(a);})(newelem$425,v$60);
return newelem$425;
} else {var attrs$432 = [["class","negative-number"],null];
var newelem$434 = document.createElement("span");
var fix$832 = {};
fix$832.$app = function(v$436){lab$app: while (true) {if (v$436 == null) {return 0;
} else {var v$437 = v$436;
var v$438 = v$437[0];
var v$439 = v$437[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$434,v$438[0],v$438[1]);
var t$833 = v$439;
var v$436 = t$833;
continue lab$app;
};
};
};
var app$435 = fix$832.$app;
app$435(attrs$432);
(function(e,a){return e.appendChild(a);})(newelem$434,v$60);
return newelem$434;
};
};
prettyprint$0prettyprint$1.decorateStr$70 = function(v$73,v$76){return v$73?(("<span class='positive-number'>" + v$76) + "</span>"):(("<span class='negative-number'>" + v$76) + "</span>");
};
var fix$834 = {};
fix$834.$mkThousandSeps = function(whole$89){return (whole$89.length > 3)?(((fix$834.$mkThousandSeps(basis$0String$1.substring$169(whole$89,0,SmlPrims.chk_ovf_i32(whole$89.length - 3)))) + ",") + (basis$0String$1.extract$116(whole$89,SmlPrims.chk_ovf_i32(whole$89.length - 3),[1]))):whole$89;
};
prettyprint$0prettyprint$1.mkThousandSeps$86 = fix$834.$mkThousandSeps;
prettyprint$0prettyprint$1.real_to_string_with_sep$94 = function(n$97,r$100){if (r$100 < 0.0) {var t$836 = "-";
var t$835;
var r$454 = -(r$100);
var s$455 = (basis$0Real$1.fmt$164([2,[0,n$97]]))(r$454);
var v$456 = basis$0String$1.tokens$224(function(c$457){return c$457 == 46;
},s$455);
if (v$456 == null) {t$835 = s$455;
} else {var v$458 = v$456;
var v$459 = v$458[1];
if (v$459 == null) {var v$465 = v$458[0];
t$835 = (prettyprint$0prettyprint$1.mkThousandSeps$86(v$465));
} else {var v$460 = v$459;
if (v$460[1] == null) {var v$461 = v$458[0];
var v$462 = v$460[0];
t$835 = (((prettyprint$0prettyprint$1.mkThousandSeps$86(v$461)) + ".") + v$462);
} else {t$835 = s$455;
};
};
};
return t$836 + t$835;
} else {var s$467 = (basis$0Real$1.fmt$164([2,[0,n$97]]))(r$100);
var v$468 = basis$0String$1.tokens$224(function(c$469){return c$469 == 46;
},s$467);
if (v$468 == null) {return s$467;
} else {var v$470 = v$468;
var v$471 = v$470[1];
if (v$471 == null) {var v$477 = v$470[0];
return prettyprint$0prettyprint$1.mkThousandSeps$86(v$477);
} else {var v$472 = v$471;
if (v$472[1] == null) {var v$473 = v$470[0];
var v$474 = v$472[0];
return ((prettyprint$0prettyprint$1.mkThousandSeps$86(v$473)) + ".") + v$474;
} else {return s$467;
};
};
};
};
};
prettyprint$0prettyprint$1.ppNumberRealStr0$133 = function(n$136,r$139){var v$478 = r$139 >= 0.0;
var v$479 = prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$136,r$139);
return v$478?(("<span class='positive-number'>" + v$479) + "</span>"):(("<span class='negative-number'>" + v$479) + "</span>");
};
prettyprint$0prettyprint$1.ppNumberRealStr$140 = function(r$143){var v$489 = r$143 >= 0.0;
var v$490 = prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$143);
return v$489?(("<span class='positive-number'>" + v$490) + "</span>"):(("<span class='negative-number'>" + v$490) + "</span>");
};
prettyprint$0prettyprint$1.ppNumberReal0$144 = function(n$147,r$150){return prettyprint$0prettyprint$1.decorate$54(r$150 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$147,r$150)));
};
prettyprint$0prettyprint$1.ppNumberReal$151 = function(r$154){return prettyprint$0prettyprint$1.decorate$54(r$154 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$154)));
};
prettyprint$0prettyprint$1.ppNumber0$155 = function(n$158,s$161){var v$166 = basis$0Real$1.fromString$497(s$161);
switch (v$166[0]) { case 0: {var v$168 = v$166[1];
return prettyprint$0prettyprint$1.decorate$54(v$168 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$158,v$168)));
 break; }default: {return document.createTextNode(s$161);
} };
};
prettyprint$0prettyprint$1.ppBillions0$169 = function(n$172,r$175){return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$172,r$175 / 1000000000.0)) + "B";
};
prettyprint$0prettyprint$1.ppBillions$176 = function(r$179){return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$179 / 1000000000.0)) + "B";
};
prettyprint$0prettyprint$1.ppMillions0$180 = function(n$183,r$186){return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$183,r$186 / 1000000.0)) + "M";
};
prettyprint$0prettyprint$1.ppMillions$187 = function(r$190){return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$190 / 1000000.0)) + "M";
};
prettyprint$0prettyprint$1.ppThousands0$191 = function(n$194,r$197){return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$197 / 1000.0)) + "k";
};
prettyprint$0prettyprint$1.ppThousands$198 = function(r$201){return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$201 / 1000.0)) + "k";
};
prettyprint$0prettyprint$1.ppNumber0$$202 = function(n$205,r$208){return prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$205,r$208);
};
prettyprint$0prettyprint$1.ppNumber$$209 = function(r$212){return prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$212);
};
prettyprint$0prettyprint$1.ppNumber$213 = function(s$216){var v$524 = basis$0Real$1.fromString$497(s$216);
switch (v$524[0]) { case 0: {var v$525 = v$524[1];
return prettyprint$0prettyprint$1.decorate$54(v$525 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$525)));
 break; }default: {return document.createTextNode(s$216);
} };
};
prettyprint$0prettyprint$1.ppNumberStr0$$217 = function(n$220,s$223){var v$228 = basis$0Real$1.fromString$497(s$223);
switch (v$228[0]) { case 0: {var v$230 = v$228[1];
return prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$220,v$230);
 break; }default: {return s$223;
} };
};
prettyprint$0prettyprint$1.ppNumberStr$$231 = function(s$234){var v$532 = basis$0Real$1.fromString$497(s$234);
switch (v$532[0]) { case 0: {var v$533 = v$532[1];
return prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$533);
 break; }default: {return s$234;
} };
};
prettyprint$0prettyprint$1.ppSmart0$235 = function(n$238,r$241){if (r$241 > 999999999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$238,r$241 / 1000000000.0)) + "B";
} else {if (r$241 > 999999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$238,r$241 / 1000000.0)) + "M";
} else {if (r$241 > 999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$241 / 1000.0)) + "k";
} else {return prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$238,r$241);
};
};
};
};
prettyprint$0prettyprint$1.ppSmart$254 = function(r$257){if (r$257 > 999999999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$257 / 1000000000.0)) + "B";
} else {if (r$257 > 999999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$257 / 1000000.0)) + "M";
} else {if (r$257 > 999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$257 / 1000.0)) + "k";
} else {return prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$257);
};
};
};
};
prettyprint$0prettyprint$1.ppSmartStr$258 = function(s$261){var v$266 = basis$0Real$1.fromString$497(s$261);
switch (v$266[0]) { case 0: {var v$268 = v$266[1];
if (v$268 > 999999999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$268 / 1000000000.0)) + "B";
} else {if (v$268 > 999999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$268 / 1000000.0)) + "M";
} else {if (v$268 > 999.0) {return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$268 / 1000.0)) + "k";
} else {return prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$268);
};
};
};
 break; }default: {return s$261;
} };
};
prettyprint$0prettyprint$1.ppPercent0$$269 = function(n$272,r$275){return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$272,r$275 * 100.0)) + "%";
};
prettyprint$0prettyprint$1.ppPercent$$276 = function(r$279){return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$279 * 100.0)) + "%";
};
prettyprint$0prettyprint$1.ppPercent$280 = function(r$283){return prettyprint$0prettyprint$1.decorate$54(r$283 >= 0.0,document.createTextNode((prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,r$283 * 100.0)) + "%"));
};
prettyprint$0prettyprint$1.ppPercentStr$$284 = function(s$287){var v$292 = basis$0Real$1.fromString$497(s$287);
switch (v$292[0]) { case 0: {var v$294 = v$292[1];
return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$294 * 100.0)) + "%";
 break; }default: {return s$287;
} };
};
prettyprint$0prettyprint$1.ppPercentStr$295 = function(s$298){var v$303 = basis$0Real$1.fromString$497(s$298);
switch (v$303[0]) { case 0: {var v$305 = v$303[1];
return prettyprint$0prettyprint$1.decorate$54(v$305 >= 0.0,document.createTextNode((prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$305 * 100.0)) + "%"));
 break; }default: {return document.createTextNode(s$298);
} };
};
prettyprint$0prettyprint$1.ppNumberOrInt$306 = function(s$309){var t$837;
var stop$570 = s$309.length;
var fix$838 = {};
fix$838.$lr = function(j$572){lab$lr: while (true) {if (j$572 < stop$570) {if ((s$309.charCodeAt(j$572)) == 46) {return true;
} else {var t$839 = SmlPrims.chk_ovf_i32(j$572 + 1);
var j$572 = t$839;
continue lab$lr;
};
} else {return false;
};
};
};
var lr$571 = fix$838.$lr;
t$837 = (lr$571(0));
if (t$837) {var v$576 = basis$0Real$1.fromString$497(s$309);
switch (v$576[0]) { case 0: {var v$577 = v$576[1];
return prettyprint$0prettyprint$1.decorate$54(v$577 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$577)));
 break; }default: {return document.createTextNode(s$309);
} };
} else {var v$318 = basis$0IntInf$1.fromString$2413(s$309);
switch (v$318[0]) { case 0: {var v$324 = v$318[1];
if (basis$0IntInfRep$1.s$lk$1480(v$324,[0,[null,false]])) {return prettyprint$0prettyprint$1.decorate$54(basis$0IntInfRep$1.s$lk$1480(v$324,[0,[null,false]]),document.createTextNode(prettyprint$0prettyprint$1.mkThousandSeps$86(s$309)));
} else {return prettyprint$0prettyprint$1.decorate$54(basis$0IntInfRep$1.s$lk$1480(v$324,[0,[null,false]]),document.createTextNode("-" + (prettyprint$0prettyprint$1.mkThousandSeps$86(basis$0IntInf$1.toString$2351(basis$0IntInfRep$1.s$p$1215(v$324))))));
};
 break; }default: {var v$584 = basis$0Real$1.fromString$497(s$309);
switch (v$584[0]) { case 0: {var v$585 = v$584[1];
return prettyprint$0prettyprint$1.decorate$54(v$585 >= 0.0,document.createTextNode(prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$585)));
 break; }default: {return document.createTextNode(s$309);
} };
} };
};
};
prettyprint$0prettyprint$1.ppNumberStr0$328 = function(n$331,s$334){var v$339 = basis$0Real$1.fromString$497(s$334);
switch (v$339[0]) { case 0: {var v$341 = v$339[1];
var v$588 = v$341 >= 0.0;
var v$589 = prettyprint$0prettyprint$1.real_to_string_with_sep$94(n$331,v$341);
return v$588?(("<span class='positive-number'>" + v$589) + "</span>"):(("<span class='negative-number'>" + v$589) + "</span>");
 break; }default: {return s$334;
} };
};
prettyprint$0prettyprint$1.ppNumberStr$342 = function(s$345){var v$809 = basis$0Real$1.fromString$497(s$345);
switch (v$809[0]) { case 0: {var v$810 = v$809[1];
var v$811 = v$810 >= 0.0;
var v$812 = prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$810);
return v$811?(("<span class='positive-number'>" + v$812) + "</span>"):(("<span class='negative-number'>" + v$812) + "</span>");
 break; }default: {return s$345;
} };
};
prettyprint$0prettyprint$1.ppNumberOrIntStr$346 = function(s$349){var t$840;
var stop$599 = s$349.length;
var fix$841 = {};
fix$841.$lr = function(j$601){lab$lr: while (true) {if (j$601 < stop$599) {if ((s$349.charCodeAt(j$601)) == 46) {return true;
} else {var t$842 = SmlPrims.chk_ovf_i32(j$601 + 1);
var j$601 = t$842;
continue lab$lr;
};
} else {return false;
};
};
};
var lr$600 = fix$841.$lr;
t$840 = (lr$600(0));
if (t$840) {var v$816 = basis$0Real$1.fromString$497(s$349);
switch (v$816[0]) { case 0: {var v$817 = v$816[1];
var v$818 = v$817 >= 0.0;
var v$819 = prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$817);
return v$818?(("<span class='positive-number'>" + v$819) + "</span>"):(("<span class='negative-number'>" + v$819) + "</span>");
 break; }default: {return s$349;
} };
} else {var v$358 = basis$0IntInf$1.fromString$2413(s$349);
switch (v$358[0]) { case 0: {var v$364 = v$358[1];
if (basis$0IntInfRep$1.s$lk$1480(v$364,[0,[null,false]])) {var v$603 = basis$0IntInfRep$1.s$lk$1480(v$364,[0,[null,false]]);
var v$604 = prettyprint$0prettyprint$1.mkThousandSeps$86(s$349);
return v$603?(("<span class='positive-number'>" + v$604) + "</span>"):(("<span class='negative-number'>" + v$604) + "</span>");
} else {var v$612 = basis$0IntInfRep$1.s$lk$1480(v$364,[0,[null,false]]);
var v$613 = "-" + (prettyprint$0prettyprint$1.mkThousandSeps$86(basis$0IntInf$1.toString$2351(basis$0IntInfRep$1.s$p$1215(v$364))));
return v$612?(("<span class='positive-number'>" + v$613) + "</span>"):(("<span class='negative-number'>" + v$613) + "</span>");
};
 break; }default: {var v$822 = basis$0Real$1.fromString$497(s$349);
switch (v$822[0]) { case 0: {var v$823 = v$822[1];
var v$824 = v$823 >= 0.0;
var v$825 = prettyprint$0prettyprint$1.real_to_string_with_sep$94(2,v$823);
return v$824?(("<span class='positive-number'>" + v$825) + "</span>"):(("<span class='negative-number'>" + v$825) + "</span>");
 break; }default: {return s$349;
} };
} };
};
};
prettyprint$0prettyprint$1.ppRatio$368 = function(r$371){return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(0,r$371)) + ":1";
};
prettyprint$0prettyprint$1.labelFormatter$372 = function(f$375){return (function(f){return function() { return f(this.name); };})(f$375);
};
prettyprint$0prettyprint$1.pointFormatter$376 = function(f$379){return (function(f){return function() { return f(this); };})(f$379);
};
prettyprint$0prettyprint$1.percentFormatter$380 = function(decimals$383){return (function(f){return function() { return f(this); };})(function(this$826){var value$827 = (function(fp,s){return fp[s];})(this$826,"value");
return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(decimals$383,value$827 * 100.0)) + "%";
});
};
prettyprint$0prettyprint$1.ratioFormatter$388 = function(decimals$391){return (function(f){return function() { return f(this); };})(function(this$828){var value$829 = (function(fp,s){return fp[s];})(this$828,"value");
return (prettyprint$0prettyprint$1.real_to_string_with_sep$94(decimals$391,value$829)) + ":1";
});
};
return 0;
})();