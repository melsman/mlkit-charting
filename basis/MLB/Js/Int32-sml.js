if ((typeof(basis$0Int32$1)) == "undefined") {basis$0Int32$1 = {};
};
(function(){basis$0Int32$1.quot$53 = function(v$62,v$63){if (v$63 == 0) {throw CompilerInitial.exn$Div$45;
} else {return SmlPrims.chk_ovf_i32(SmlPrims.quot(v$62,v$63));
};
};
basis$0Int32$1.rem$64 = function(v$73,v$74){if (v$74 == 0) {throw CompilerInitial.exn$Div$45;
} else {return v$73 % v$74;
};
};
basis$0Int32$1.toLarge$83 = function(x$86){return basis$0IntInfRep$1.fromInt32$1043(x$86);
};
basis$0Int32$1.fromLarge$87 = function(x$90){return basis$0IntInfRep$1.toInt32$1031(x$90);
};
basis$0Int32$1.toInt$91 = function(x$94){return x$94;
};
basis$0Int32$1.fromInt$95 = function(x$98){return x$98;
};
basis$0Int32$1.precision$99 = [0,32];
basis$0Int32$1.maxInt$100 = [0,2147483647];
basis$0Int32$1.minInt$101 = [0,-2147483648];
basis$0Int32$1.s$p$102 = function(v$103){return SmlPrims.chk_ovf_i32(-(v$103));
};
basis$0Int32$1.s$t$104 = function(v$633,v$634){return SmlPrims.chk_ovf_i32(v$633 * v$634);
};
basis$0Int32$1.div$106 = function(v$635,v$636){return SmlPrims.div_i32(v$635,v$636,CompilerInitial.exn$Div$45);
};
basis$0Int32$1.mod$108 = function(v$637,v$638){return SmlPrims.mod_i32(v$637,v$638,CompilerInitial.exn$Div$45);
};
basis$0Int32$1.s$f$110 = function(v$639,v$640){return SmlPrims.chk_ovf_i32(v$639 + v$640);
};
basis$0Int32$1.s$g$112 = function(v$641,v$642){return SmlPrims.chk_ovf_i32(v$641 - v$642);
};
basis$0Int32$1.compare$114 = function(v$127,v$128){return (v$127 < v$128)?0:((v$127 > v$128)?1:2);
};
basis$0Int32$1.abs$129 = function(v$130){return SmlPrims.chk_ovf_i32(Math.abs(v$130));
};
basis$0Int32$1.min$131 = function(v$140,v$141){return (v$140 < v$141)?v$140:v$141;
};
basis$0Int32$1.max$142 = function(v$151,v$152){return (v$151 < v$152)?v$152:v$151;
};
basis$0Int32$1.sign$153 = function(i$156){return (i$156 > 0)?1:((i$156 < 0)?(-1):0);
};
basis$0Int32$1.sameSign$165 = function(v$170,v$171){return ((v$170 > 0)?1:((v$170 < 0)?(-1):0)) == ((v$171 > 0)?1:((v$171 < 0)?(-1):0));
};
var conv$207 = function(radix$210,i$213){var t$674;
var v$577 = [0,i$213];
var v$665 = [v$577,basis$0Int32$1.minInt$101];
t$674 = (basis$0General$1.eq_option$256(function(v$666){return v$666[0] == v$666[1];
},v$665));
if (t$674) {switch (radix$210) { case 2: {return "~10000000000000000000000000000000";
 break; }case 8: {return "~20000000000";
 break; }case 10: {return "~2147483648";
 break; }case 16: {return "~80000000";
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"conv"];
} };
} else {var fix$667 = {};
fix$667.$h = function(v$231,v$234){lab$h: while (true) {switch (v$231) { case 0: {return v$234;
 break; }default: {var t$671 = SmlPrims.div_i32(v$231,radix$210,CompilerInitial.exn$Div$45);
var t$670;
var t$669;
var t$668;
var i$517 = SmlPrims.mod_i32(v$231,radix$210,CompilerInitial.exn$Div$45);
if (i$517 < 10) {var i$518 = SmlPrims.chk_ovf_i32(i$517 + 48);
t$668 = (basis$0Char$1.chr$69(i$518));
} else {var i$520 = SmlPrims.chk_ovf_i32(i$517 + 55);
t$668 = (basis$0Char$1.chr$69(i$520));
};
t$669 = [t$668,v$234];
t$670 = t$669;
var t$672 = t$671;
var t$673 = t$670;
var v$231 = t$672;
var v$234 = t$673;
continue lab$h;
} };
};
};
var h$228 = fix$667.$h;
var t$688;
if (i$213 < 0) {var t$682;
var t$681 = 126;
var t$675;
var n$625 = SmlPrims.chk_ovf_i32(-(i$213));
var t$680 = h$228;
var t$679 = SmlPrims.div_i32(n$625,radix$210,CompilerInitial.exn$Div$45);
var t$678;
var t$677;
var t$676;
var i$626 = SmlPrims.mod_i32(n$625,radix$210,CompilerInitial.exn$Div$45);
if (i$626 < 10) {var i$627 = SmlPrims.chk_ovf_i32(i$626 + 48);
t$676 = (basis$0Char$1.chr$69(i$627));
} else {var i$628 = SmlPrims.chk_ovf_i32(i$626 + 55);
t$676 = (basis$0Char$1.chr$69(i$628));
};
t$677 = [t$676,null];
t$678 = t$677;
t$675 = (t$680(t$679,t$678));
t$682 = [t$681,t$675];
t$688 = t$682;
} else {var t$687 = h$228;
var t$686 = SmlPrims.div_i32(i$213,radix$210,CompilerInitial.exn$Div$45);
var t$685;
var t$684;
var t$683;
var i$630 = SmlPrims.mod_i32(i$213,radix$210,CompilerInitial.exn$Div$45);
if (i$630 < 10) {var i$631 = SmlPrims.chk_ovf_i32(i$630 + 48);
t$683 = (basis$0Char$1.chr$69(i$631));
} else {var i$632 = SmlPrims.chk_ovf_i32(i$630 + 55);
t$683 = (basis$0Char$1.chr$69(i$632));
};
t$684 = [t$683,null];
t$685 = t$684;
t$688 = (t$687(t$686,t$685));
};
return SmlPrims.implode(t$688);
};
};
basis$0Int32$1.scan$253 = function(radix$256,getc$259,source$262){var v$417;
switch (radix$256) { case 3: {v$417 = [function(c$437){return (48 <= c$437)?(c$437 <= 49):false;
},2];
 break; }case 0: {v$417 = [function(c$430){return (48 <= c$430)?(c$430 <= 55):false;
},8];
 break; }case 2: {v$417 = [function(c$532){return (48 <= c$532)?(c$532 <= 57):false;
},10];
 break; }default: {v$417 = [function(c$533){return ((48 <= c$533)?(c$533 <= 57):false)?true:(((97 <= c$533)?(c$533 <= 102):false)?true:((65 <= c$533)?(c$533 <= 70):false));
},16];
} };
var v$418 = v$417[0];
var v$419 = v$417[1];
var dig1$266 = function(v$269,v$272){switch (v$272[0]) { case 1: {return [1];
 break; }default: {var v$332 = v$272[1];
var v$333 = v$332[0];
var v$334 = v$332[1];
var fix$689 = {};
fix$689.$digr = function(res$286,next_val$289,src$292){lab$digr: while (true) {var v$298 = getc$259(src$292);
switch (v$298[0]) { case 1: {return [0,[res$286,src$292]];
 break; }default: {var v$303 = v$298[1];
var v$304 = v$303[0];
var v$305 = v$303[1];
if (v$418(v$304)) {var t$690 = next_val$289([v$419,res$286,((48 <= v$304)?(v$304 <= 57):false)?(SmlPrims.chk_ovf_i32(v$304 - 48)):(SmlPrims.mod_i32(SmlPrims.chk_ovf_i32(v$304 - 55),32,CompilerInitial.exn$Div$45))]);
var t$691 = next_val$289;
var t$692 = v$305;
var res$286 = t$690;
var next_val$289 = t$691;
var src$292 = t$692;
continue lab$digr;
} else {return [0,[res$286,src$292]];
};
} };
};
};
var digr$283 = fix$689.$digr;
var next_val$306 = (v$269 == 1)?(function(v$315){var v$316 = v$315[0];
var v$317 = v$315[1];
var v$318 = v$315[2];
return SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$316 * v$317)) + v$318);
}):(function(v$323){var v$324 = v$323[0];
var v$325 = v$323[1];
var v$326 = v$323[2];
return SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$324 * v$325)) - v$326);
});
if (v$418(v$333)) {return digr$283(SmlPrims.chk_ovf_i32(v$269 * (((48 <= v$333)?(v$333 <= 57):false)?(SmlPrims.chk_ovf_i32(v$333 - 48)):(SmlPrims.mod_i32(SmlPrims.chk_ovf_i32(v$333 - 55),32,CompilerInitial.exn$Div$45)))),next_val$306,v$334);
} else {return [1];
};
} };
};
var hexopt$350 = function(v$353,v$356){switch (v$356[0]) { case 1: {return [1];
 break; }default: {var v$370 = v$356[1];
switch (v$370[0]) { case 48: {var v$392 = v$370[1];
var t$694;
var t$693;
var v$624 = 1;
t$693 = (basis$0StringCvt$1.eq_radix$302(radix$256,v$624));
t$694 = (t$693?false:true);
if (t$694) {var inp$554 = getc$259(v$392);
var v$555 = dig1$266(v$353,inp$554);
switch (v$555[0]) { case 1: {return [0,[0,v$392]];
 break; }default: {return v$555;
} };
} else {var v$386 = getc$259(v$392);
switch (v$386[0]) { case 1: {return [0,[0,v$392]];
 break; }default: {var v$387 = v$386[1];
switch (v$387[0]) { case 120: {var v$390 = v$387[1];
var inp$558 = getc$259(v$390);
var v$559 = dig1$266(v$353,inp$558);
switch (v$559[0]) { case 1: {return [0,[0,v$392]];
 break; }default: {return v$559;
} };
 break; }case 88: {var v$389 = v$387[1];
var inp$562 = getc$259(v$389);
var v$563 = dig1$266(v$353,inp$562);
switch (v$563[0]) { case 1: {return [0,[0,v$392]];
 break; }default: {return v$563;
} };
 break; }default: {var v$567 = dig1$266(v$353,v$386);
switch (v$567[0]) { case 1: {return [0,[0,v$392]];
 break; }default: {return v$567;
} };
} };
} };
};
 break; }default: {return dig1$266(v$353,v$356);
} };
} };
};
var v$568 = getc$259(basis$0StringCvt$1.dropl$246(function(c$575){return (c$575 == 32)?true:((9 <= c$575)?(c$575 <= 13):false);
},getc$259,source$262));
switch (v$568[0]) { case 1: {return [1];
 break; }default: {var v$569 = v$568[1];
switch (v$569[0]) { case 126: {var v$570 = v$569[1];
return hexopt$350(-1,getc$259(v$570));
 break; }case 45: {var v$571 = v$569[1];
return hexopt$350(-1,getc$259(v$571));
 break; }case 43: {var v$572 = v$569[1];
return hexopt$350(1,getc$259(v$572));
 break; }default: {return hexopt$350(1,v$568);
} };
} };
};
basis$0Int32$1.fmt$442 = function(v$445){switch (v$445) { case 3: {return function(v$654){return conv$207(2,v$654);
};
 break; }case 0: {return function(v$656){return conv$207(8,v$656);
};
 break; }case 2: {return function(v$658){return conv$207(10,v$658);
};
 break; }default: {return function(v$660){return conv$207(16,v$660);
};
} };
};
basis$0Int32$1.toString$454 = function(i$457){return conv$207(10,i$457);
};
basis$0Int32$1.fromString$458 = function(s$461){var t$696 = basis$0StringCvt$1.scanString$159;
var t$695;
var v$661 = 2;
t$695 = (function(v$662){return function(v$663){return basis$0Int32$1.scan$253(v$661,v$662,v$663);
};
});
return t$696(t$695,s$461);
};
basis$0Int32$1.s$l$462 = function(v$643,v$644){return v$643 > v$644;
};
basis$0Int32$1.s$lk$464 = function(v$645,v$646){return v$645 >= v$646;
};
basis$0Int32$1.s$j$466 = function(v$647,v$648){return v$647 < v$648;
};
basis$0Int32$1.s$jk$468 = function(v$649,v$650){return v$649 <= v$650;
};
return 0;
})();
