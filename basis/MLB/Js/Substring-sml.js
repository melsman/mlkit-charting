if ((typeof(basis$0Substring$1)) == "undefined") {basis$0Substring$1 = {};
};
(function(){var blit_unsafe$81 = function(v$104,v$105,v$106,v$107,v$108){var fix$1084 = {};
fix$1084.$loop = function(v$101,v$102,v$103){lab$loop: while (true) {if (v$103 > 0) {(v$106[v$102] = (v$104.charCodeAt(v$101)),0);
var t$1085 = SmlPrims.chk_ovf_i32(v$101 + 1);
var t$1086 = SmlPrims.chk_ovf_i32(v$102 + 1);
var t$1087 = SmlPrims.chk_ovf_i32(v$103 - 1);
var v$101 = t$1085;
var v$102 = t$1086;
var v$103 = t$1087;
continue lab$loop;
} else {return 0;
};
};
};
var loop$89 = fix$1084.$loop;
return loop$89(v$105,v$107,v$108);
};
basis$0Substring$1.base$109 = function(arg$112){return arg$112;
};
basis$0Substring$1.string$113 = function(v$122,v$123,v$124){var newstr$119 = Array(v$124);
blit_unsafe$81(v$122,v$123,newstr$119,0,v$124);
return SmlPrims.charArrayToString(newstr$119);
};
basis$0Substring$1.extract$125 = function(v$1051,v$1052,v$138){switch (v$138[0]) { case 1: {if ((0 <= v$1052)?(v$1052 <= v$1051.length):false) {return [v$1051,v$1052,SmlPrims.chk_ovf_i32(v$1051.length - v$1052)];
} else {throw basis$0General$1.exn$Subscript$56;
};
 break; }default: {var v$163 = v$138[1];
if ((0 <= v$1052)?((0 <= v$163)?(v$163 <= (SmlPrims.chk_ovf_i32(v$1051.length - v$1052))):false):false) {return [v$1051,v$1052,v$163];
} else {throw basis$0General$1.exn$Subscript$56;
};
} };
};
basis$0Substring$1.substring$164 = function(v$170,v$171,v$172){return basis$0Substring$1.extract$125(v$170,v$171,[0,v$172]);
};
basis$0Substring$1.full$173 = function(s$176){return [s$176,0,s$176.length];
};
basis$0Substring$1.getc$177 = function(v$1053,v$1054,v$190){switch (v$190) { case 0: {return [1];
 break; }default: {return [0,[v$1053.charCodeAt(v$1054),[v$1053,SmlPrims.chk_ovf_i32(v$1054 + 1),SmlPrims.chk_ovf_i32(v$190 - 1)]]];
} };
};
basis$0Substring$1.first$195 = function(v$205,v$206,v$207){return (v$207 == 0)?[1]:[0,v$205.charCodeAt(v$206)];
};
basis$0Substring$1.isEmpty$208 = function(v$1055,v$1056,v$1057){return v$1057 == 0;
};
basis$0Substring$1.triml$217 = function(k$220){if (k$220 < 0) {throw basis$0General$1.exn$Subscript$56;
} else {return function(v$229){var v$234 = v$229[0];
var v$235 = v$229[1];
var v$236 = v$229[2];
return (k$220 > v$236)?[v$234,SmlPrims.chk_ovf_i32(v$235 + v$236),0]:[v$234,SmlPrims.chk_ovf_i32(v$235 + k$220),SmlPrims.chk_ovf_i32(v$236 - k$220)];
};
};
};
basis$0Substring$1.trimr$237 = function(k$240){if (k$240 < 0) {throw basis$0General$1.exn$Subscript$56;
} else {return function(v$249){var v$254 = v$249[0];
var v$255 = v$249[1];
var v$256 = v$249[2];
return (k$240 > v$256)?[v$254,v$255,0]:[v$254,v$255,SmlPrims.chk_ovf_i32(v$256 - k$240)];
};
};
};
basis$0Substring$1.sub$257 = function(v$272,v$1058){var v$273 = v$272[0];
var v$274 = v$272[1];
var v$275 = v$272[2];
if ((v$1058 < 0)?true:(v$1058 >= v$275)) {throw basis$0General$1.exn$Subscript$56;
} else {return v$273.charCodeAt(SmlPrims.chk_ovf_i32(v$274 + v$1058));
};
};
basis$0Substring$1.size$277 = function(v$1059,v$1060,v$1061){return v$1061;
};
basis$0Substring$1.slice$282 = function(v$1062,v$1063,v$299){switch (v$299[0]) { case 1: {var v$309 = v$1062[0];
var v$310 = v$1062[1];
var v$311 = v$1062[2];
if ((0 <= v$1063)?(v$1063 <= v$311):false) {return [v$309,SmlPrims.chk_ovf_i32(v$310 + v$1063),SmlPrims.chk_ovf_i32(v$311 - v$1063)];
} else {throw basis$0General$1.exn$Subscript$56;
};
 break; }default: {var v$326 = v$1062[0];
var v$327 = v$1062[1];
var v$328 = v$1062[2];
var v$330 = v$299[1];
if ((0 <= v$1063)?((0 <= v$330)?((SmlPrims.chk_ovf_i32(v$1063 + v$330)) <= v$328):false):false) {return [v$326,SmlPrims.chk_ovf_i32(v$327 + v$1063),v$330];
} else {throw basis$0General$1.exn$Subscript$56;
};
} };
};
basis$0Substring$1.splitAt$331 = function(v$346,v$1064){var v$347 = v$346[0];
var v$348 = v$346[1];
var v$349 = v$346[2];
if ((v$1064 < 0)?true:(v$1064 > v$349)) {throw basis$0General$1.exn$Subscript$56;
} else {return [[v$347,v$348,v$1064],[v$347,SmlPrims.chk_ovf_i32(v$348 + v$1064),SmlPrims.chk_ovf_i32(v$349 - v$1064)]];
};
};
basis$0Substring$1.concat$351 = function(strs$354){var fix$1088 = {};
fix$1088.$acc = function(v$358,v$361){lab$acc: while (true) {if (v$358 == null) {return v$361;
} else {var v$372 = v$358;
var v$373 = v$372[0];
var v$374 = v$373[2];
var v$375 = v$372[1];
var t$1089 = v$375;
var t$1090 = SmlPrims.chk_ovf_i32(v$374 + v$361);
var v$358 = t$1089;
var v$361 = t$1090;
continue lab$acc;
};
};
};
var acc$355 = fix$1088.$acc;
var len$377 = acc$355(strs$354,0);
var newstr$378;
if (len$377 > 16777211) {throw basis$0General$1.exn$Size$58;
} else {newstr$378 = (Array(len$377));
};
var fix$1091 = {};
fix$1091.$copyall = function(v$386,v$389){lab$copyall: while (true) {if (v$389 == null) {return 0;
} else {var v$405 = v$389;
var v$406 = v$405[0];
var v$407 = v$406[0];
var v$408 = v$406[1];
var v$409 = v$406[2];
var v$410 = v$405[1];
blit_unsafe$81(v$407,v$408,newstr$378,v$386,v$409);
var t$1092 = SmlPrims.chk_ovf_i32(v$386 + v$409);
var t$1093 = v$410;
var v$386 = t$1092;
var v$389 = t$1093;
continue lab$copyall;
};
};
};
var copyall$383 = fix$1091.$copyall;
copyall$383(0,strs$354);
return SmlPrims.charArrayToString(newstr$378);
};
basis$0Substring$1.concatWith$413 = function(v$416,v$419){if (v$419 == null) {return "";
} else {var v$495 = v$419;
var v$496 = v$495[0];
var v$497 = v$496[0];
var v$498 = v$496[1];
var v$499 = v$496[2];
var v$500 = v$495[1];
var seplen$432 = v$416.length;
var fix$1094 = {};
fix$1094.$acc = function(v$436,v$439){lab$acc: while (true) {if (v$436 == null) {return v$439;
} else {var v$450 = v$436;
var v$451 = v$450[0];
var v$452 = v$450[1];
var t$1095 = v$452;
var t$1096 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$451[2] + seplen$432)) + v$439);
var v$436 = t$1095;
var v$439 = t$1096;
continue lab$acc;
};
};
};
var acc$433 = fix$1094.$acc;
var len$454 = acc$433(v$500,v$499);
var newstr$455;
if (len$454 > 16777211) {throw basis$0General$1.exn$Size$58;
} else {newstr$455 = (Array(len$454));
};
var fix$1097 = {};
fix$1097.$copyall = function(v$463,v$466){lab$copyall: while (true) {if (v$466 == null) {return 0;
} else {var v$484 = v$466;
var v$485 = v$484[0];
var v$486 = v$485[0];
var v$487 = v$485[1];
var v$488 = v$485[2];
var v$489 = v$484[1];
blit_unsafe$81(v$416,0,newstr$455,v$463,seplen$432);
blit_unsafe$81(v$486,v$487,newstr$455,SmlPrims.chk_ovf_i32(v$463 + seplen$432),v$488);
var t$1098 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$463 + seplen$432)) + v$488);
var t$1099 = v$489;
var v$463 = t$1098;
var v$466 = t$1099;
continue lab$copyall;
};
};
};
var copyall$460 = fix$1097.$copyall;
blit_unsafe$81(v$497,v$498,newstr$455,0,v$499);
copyall$460(v$499,v$500);
return SmlPrims.charArrayToString(newstr$455);
};
};
basis$0Substring$1.compare$501 = function(v$541,v$1065){var v$542 = v$541[0];
var v$543 = v$541[1];
var v$544 = v$541[2];
var v$546 = v$1065[0];
var v$547 = v$1065[1];
var v$548 = v$1065[2];
var stop$510 = (v$544 < v$548)?v$544:v$548;
var fix$1100 = {};
fix$1100.$h = function(j$518){lab$h: while (true) {if (j$518 == stop$510) {return (v$544 < v$548)?0:((v$544 > v$548)?1:2);
} else {var c1$531 = v$542.charCodeAt(SmlPrims.chk_ovf_i32(v$543 + j$518));
var c2$532 = v$546.charCodeAt(SmlPrims.chk_ovf_i32(v$547 + j$518));
if (c1$531 < c2$532) {return 0;
} else {if (c1$531 > c2$532) {return 1;
} else {var t$1101 = SmlPrims.chk_ovf_i32(j$518 + 1);
var j$518 = t$1101;
continue lab$h;
};
};
};
};
};
var h$515 = fix$1100.$h;
return h$515(0);
};
basis$0Substring$1.isPrefix$549 = function(s1$552,v$557){var v$580 = v$557[0];
var v$581 = v$557[1];
var v$582 = v$557[2];
var n1$558 = s1$552.length;
var stop$559 = (n1$558 < v$582)?n1$558:v$582;
var fix$1102 = {};
fix$1102.$h = function(j$567){lab$h: while (true) {if (j$567 == stop$559) {return true;
} else {if ((s1$552.charCodeAt(j$567)) == (v$580.charCodeAt(SmlPrims.chk_ovf_i32(v$581 + j$567)))) {var t$1103 = SmlPrims.chk_ovf_i32(j$567 + 1);
var j$567 = t$1103;
continue lab$h;
} else {return false;
};
};
};
};
var h$564 = fix$1102.$h;
if (n1$558 <= v$582) {return h$564(0);
} else {return false;
};
};
basis$0Substring$1.isSuffix$583 = function(s1$586,v$591){var v$610 = v$591[0];
var v$611 = v$591[1];
var v$612 = v$591[2];
var n1$592 = s1$586.length;
var offset$593 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$611 + v$612)) - n1$592);
var fix$1104 = {};
fix$1104.$h = function(j$597){lab$h: while (true) {if (j$597 == n1$592) {return true;
} else {if ((s1$586.charCodeAt(j$597)) == (v$610.charCodeAt(SmlPrims.chk_ovf_i32(j$597 + offset$593)))) {var t$1105 = SmlPrims.chk_ovf_i32(j$597 + 1);
var j$597 = t$1105;
continue lab$h;
} else {return false;
};
};
};
};
var h$594 = fix$1104.$h;
if (n1$592 <= v$612) {return h$594(0);
} else {return false;
};
};
basis$0Substring$1.isSubstring$613 = function(v$616,v$619){switch (v$616) { case "": {return true;
 break; }default: {var v$674 = v$619[0];
var v$675 = v$619[1];
var v$676 = v$619[2];
var n1$638 = v$616.length;
var stop1$639 = SmlPrims.chk_ovf_i32(n1$638 - 1);
var stop2$640 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$675 + v$676)) - n1$638);
var fix$1106 = {};
fix$1106.$issub = function(offset$660){lab$issub: while (true) {if (offset$660 <= stop2$640) {var t$1109;
if ((v$616.charCodeAt(stop1$639)) == (v$674.charCodeAt(SmlPrims.chk_ovf_i32(stop1$639 + offset$660)))) {var fix$1107 = {};
fix$1107.$h = function(j$958){lab$h: while (true) {if (j$958 >= stop1$639) {return true;
} else {if ((v$616.charCodeAt(j$958)) == (v$674.charCodeAt(SmlPrims.chk_ovf_i32(j$958 + offset$660)))) {var t$1108 = SmlPrims.chk_ovf_i32(j$958 + 1);
var j$958 = t$1108;
continue lab$h;
} else {return false;
};
};
};
};
var h$957 = fix$1107.$h;
t$1109 = (h$957(0));
} else {t$1109 = false;
};
if (t$1109) {return true;
} else {var t$1110 = SmlPrims.chk_ovf_i32(offset$660 + 1);
var offset$660 = t$1110;
continue lab$issub;
};
} else {return false;
};
};
};
var issub$657 = fix$1106.$issub;
return issub$657(v$675);
} };
};
basis$0Substring$1.collate$677 = function(cmp$680,v$688){var v$716 = v$688[0];
var v$717 = v$716[0];
var v$718 = v$716[1];
var v$719 = v$716[2];
var v$720 = v$688[1];
var v$721 = v$720[0];
var v$722 = v$720[1];
var v$723 = v$720[2];
var stop$689 = (v$719 < v$723)?v$719:v$723;
var fix$1111 = {};
fix$1111.$h = function(j$697){lab$h: while (true) {if (j$697 == stop$689) {return (v$719 < v$723)?0:((v$719 > v$723)?1:2);
} else {switch (cmp$680([v$717.charCodeAt(SmlPrims.chk_ovf_i32(v$718 + j$697)),v$721.charCodeAt(SmlPrims.chk_ovf_i32(v$722 + j$697))])) { case 0: {return 0;
 break; }case 1: {return 1;
 break; }default: {var t$1112 = SmlPrims.chk_ovf_i32(j$697 + 1);
var j$697 = t$1112;
continue lab$h;
} };
};
};
};
var h$694 = fix$1111.$h;
return h$694(0);
};
basis$0Substring$1.foldl$724 = function(f$727,e$730,sus$733){var v$966 = sus$733[0];
var v$967 = sus$733[1];
var v$968 = sus$733[2];
var stop$969 = SmlPrims.chk_ovf_i32(v$967 + v$968);
var fix$1113 = {};
fix$1113.$h = function(v$972,v$973){lab$h: while (true) {if (v$972 >= stop$969) {return v$973;
} else {var t$1114 = SmlPrims.chk_ovf_i32(v$972 + 1);
var t$1115 = f$727([v$966.charCodeAt(v$972),v$973]);
var v$972 = t$1114;
var v$973 = t$1115;
continue lab$h;
};
};
};
var h$970 = fix$1113.$h;
return h$970(v$967,e$730);
};
basis$0Substring$1.foldr$734 = function(f$737,e$740,v$745){var v$757 = v$745[0];
var v$758 = v$745[1];
var v$759 = v$745[2];
var fix$1116 = {};
fix$1116.$h = function(j$749,res$752){lab$h: while (true) {if (j$749 < v$758) {return res$752;
} else {var t$1117 = SmlPrims.chk_ovf_i32(j$749 - 1);
var t$1118 = f$737([v$757.charCodeAt(j$749),res$752]);
var j$749 = t$1117;
var res$752 = t$1118;
continue lab$h;
};
};
};
var h$746 = fix$1116.$h;
return h$746(SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$758 + v$759)) - 1),e$740);
};
basis$0Substring$1.explode$760 = function(v$777,v$778,v$779){var fix$1119 = {};
fix$1119.$h = function(j$769,res$772){lab$h: while (true) {if (j$769 < v$778) {return res$772;
} else {var t$1120 = SmlPrims.chk_ovf_i32(j$769 - 1);
var t$1121 = [v$777.charCodeAt(j$769),res$772];
var j$769 = t$1120;
var res$772 = t$1121;
continue lab$h;
};
};
};
var h$766 = fix$1119.$h;
return h$766(SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$778 + v$779)) - 1),null);
};
basis$0Substring$1.app$780 = function(f$783,v$788){var v$800 = v$788[0];
var v$801 = v$788[1];
var v$802 = v$788[2];
var stop$789 = SmlPrims.chk_ovf_i32(v$801 + v$802);
var fix$1122 = {};
fix$1122.$h = function(j$793){lab$h: while (true) {if (j$793 >= stop$789) {return 0;
} else {f$783(v$800.charCodeAt(j$793));
var t$1123 = SmlPrims.chk_ovf_i32(j$793 + 1);
var j$793 = t$1123;
continue lab$h;
};
};
};
var h$790 = fix$1122.$h;
return h$790(v$801);
};
basis$0Substring$1.span$803 = function(v$925,v$811){var v$820 = v$811[0];
var v$821 = v$820[0];
var v$822 = v$820[1];
var v$824 = v$811[1];
var v$825 = v$824[0];
var v$826 = v$824[1];
var v$827 = v$824[2];
if ((v$822 > (SmlPrims.chk_ovf_i32(v$826 + v$827)))?true:((v$925([v$821,v$825]))?false:true)) {throw basis$0General$1.exn$Span$57;
} else {return [v$821,v$822,SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$826 + v$827)) - v$822)];
};
};
basis$0Substring$1.splitl$828 = function(v$1066,v$1067){return basis$0StrBase$1.splitl$250(v$1066,v$1067);
};
basis$0Substring$1.splitr$829 = function(v$1068,v$1069){return basis$0StrBase$1.splitr$267(v$1068,v$1069);
};
basis$0Substring$1.dropl$830 = function(v$1070,v$1071){return basis$0StrBase$1.dropl$182(v$1070,v$1071);
};
basis$0Substring$1.dropr$831 = function(v$1072,v$1073){return basis$0StrBase$1.dropr$199(v$1072,v$1073);
};
basis$0Substring$1.takel$832 = function(v$1074,v$1075){return basis$0StrBase$1.takel$216(v$1074,v$1075);
};
basis$0Substring$1.taker$833 = function(v$1076,v$1077){return basis$0StrBase$1.taker$233(v$1076,v$1077);
};
basis$0Substring$1.translate$834 = function(v$1078,v$1079){return basis$0StrBase$1.translate$284(v$1078,v$1079);
};
basis$0Substring$1.tokens$835 = function(v$1080,v$1081){return basis$0StrBase$1.tokens$308(v$1080,v$1081);
};
basis$0Substring$1.fields$836 = function(v$1082,v$1083){return basis$0StrBase$1.fields$341(v$1082,v$1083);
};
basis$0Substring$1.position$837 = function(v$840,v$843){switch (v$840) { case "": {return [[v$843[0],v$843[1],0],v$843];
 break; }default: {var v$896 = v$843[0];
var v$897 = v$843[1];
var v$898 = v$843[2];
var len1$862 = SmlPrims.chk_ovf_i32(v$840.length - 1);
var fix$1124 = {};
fix$1124.$eq = function(j$866,k$869){lab$eq: while (true) {if (j$866 >= len1$862) {return true;
} else {if ((v$840.charCodeAt(j$866)) == (v$896.charCodeAt(k$869))) {var t$1125 = SmlPrims.chk_ovf_i32(j$866 + 1);
var t$1126 = SmlPrims.chk_ovf_i32(k$869 + 1);
var j$866 = t$1125;
var k$869 = t$1126;
continue lab$eq;
} else {return false;
};
};
};
};
var eq$863 = fix$1124.$eq;
var stop$878 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$897 + v$898)) - len1$862)) - 1);
var fix$1127 = {};
fix$1127.$cmp = function(k$882){lab$cmp: while (true) {if (k$882 > stop$878) {return [v$843,[v$896,SmlPrims.chk_ovf_i32(v$897 + v$898),0]];
} else {if (((v$840.charCodeAt(len1$862)) == (v$896.charCodeAt(SmlPrims.chk_ovf_i32(k$882 + len1$862))))?(eq$863(0,k$882)):false) {return [[v$896,v$897,SmlPrims.chk_ovf_i32(k$882 - v$897)],[v$896,k$882,SmlPrims.chk_ovf_i32(v$898 - (SmlPrims.chk_ovf_i32(k$882 - v$897)))]];
} else {var t$1128 = SmlPrims.chk_ovf_i32(k$882 + 1);
var k$882 = t$1128;
continue lab$cmp;
};
};
};
};
var cmp$879 = fix$1127.$cmp;
return cmp$879(v$897);
} };
};
return 0;
})();
