if ((typeof(basis$0Vector$1$3)) == "undefined") {basis$0Vector$1$3 = {};
};
(function(){basis$0Vector$1$3.sub0$102 = function(v$993,v$994){return v$993[v$994];
};
basis$0Vector$1$3.sub_vector0$109 = function(v$995,v$996){return v$995[v$996];
};
basis$0Vector$1$3.update0$116 = function(v$997,v$998,v$999){return (v$997[v$998] = v$999,0);
};
basis$0Vector$1$3.update_vector0$125 = function(v$1000,v$1001,v$1002){return (v$1000[v$1001] = v$1002,0);
};
basis$0Vector$1$3.table0$134 = function(n$137){return Array(n$137);
};
basis$0Vector$1$3.vector0$138 = function(n$141){return Array(n$141);
};
basis$0Vector$1$3.length$142 = function(t$145){return t$145.length;
};
basis$0Vector$1$3.length_vector$146 = function(t$149){return t$149.length;
};
basis$0Vector$1$3.array0$150 = function(v$1003,v$1004){return SmlPrims.wordTableInit(v$1003,v$1004);
};
basis$0Vector$1$3.maxLen$157 = 123456789;
basis$0Vector$1$3.check_index$158 = function(v$171,v$172){if ((0 <= v$172)?(v$172 < v$171):false) {return 0;
} else {throw basis$0General$1.exn$Subscript$56;
};
};
basis$0Vector$1$3.check_size$173 = function(n$176){if ((0 <= n$176)?(n$176 <= 123456789):false) {return 0;
} else {throw basis$0General$1.exn$Size$58;
};
};
basis$0Vector$1$3.sub$185 = function(v$192,v$193){var v$892 = v$192.length;
if ((0 <= v$193)?(v$193 < v$892):false) {0;
} else {throw basis$0General$1.exn$Subscript$56;
};
return v$192[v$193];
};
basis$0Vector$1$3.update$194 = function(v$202,v$203,v$204){var v$896 = v$202.length;
if ((0 <= v$203)?(v$203 < v$896):false) {0;
} else {throw basis$0General$1.exn$Subscript$56;
};
return (v$202[v$203] = v$204,0);
};
basis$0Vector$1$3.table$205 = function(n$208){if ((0 <= n$208)?(n$208 <= 123456789):false) {0;
} else {throw basis$0General$1.exn$Size$58;
};
return Array(n$208);
};
basis$0Vector$1$3.vectorv$211 = function(n$214){if ((0 <= n$214)?(n$214 <= 123456789):false) {0;
} else {throw basis$0General$1.exn$Size$58;
};
return Array(n$214);
};
basis$0Vector$1$3.array$217 = function(v$224,v$225){if ((0 <= v$224)?(v$224 <= 123456789):false) {0;
} else {throw basis$0General$1.exn$Size$58;
};
return SmlPrims.wordTableInit(v$224,v$225);
};
basis$0Vector$1$3.fromList$226 = function(xs$229){var fix$1009 = {};
fix$1009.$init = function(v$1005,v$244,v$1006){lab$init: while (true) {if (v$244 == null) {return v$1005;
} else {var v$250 = v$244;
var v$251 = v$250[0];
var v$252 = v$250[1];
(v$1005[v$1006] = v$251,0);
var t$1010 = v$1005;
var t$1011 = v$252;
var t$1012 = SmlPrims.chk_ovf_i32(v$1006 + 1);
var v$1005 = t$1010;
var v$244 = t$1011;
var v$1006 = t$1012;
continue lab$init;
};
};
};
var init$230 = fix$1009.$init;
var n$254;
var fix$1013 = {};
fix$1013.$acc = function(v$818,v$819){lab$acc: while (true) {if (v$818 == null) {return v$819;
} else {var v$820 = v$818;
var v$821 = v$820[1];
var t$1014 = v$821;
var t$1015 = SmlPrims.chk_ovf_i32(v$819 + 1);
var v$818 = t$1014;
var v$819 = t$1015;
continue lab$acc;
};
};
};
var acc$817 = fix$1013.$acc;
n$254 = (acc$817(xs$229,0));
var t$1017 = init$230;
var t$1016;
if ((0 <= n$254)?(n$254 <= 123456789):false) {0;
} else {throw basis$0General$1.exn$Size$58;
};
t$1016 = (Array(n$254));
return t$1017(t$1016,xs$229,0);
};
basis$0Vector$1$3.tabulate$255 = function(v$275,v$276){var fix$1018 = {};
fix$1018.$init = function(v$272,v$273,v$274){lab$init: while (true) {if (v$274 >= v$275) {return v$272;
} else {(v$272[v$274] = (v$273(v$274)),0);
var t$1019 = v$272;
var t$1020 = v$273;
var t$1021 = SmlPrims.chk_ovf_i32(v$274 + 1);
var v$272 = t$1019;
var v$273 = t$1020;
var v$274 = t$1021;
continue lab$init;
};
};
};
var init$260 = fix$1018.$init;
var t$1023 = init$260;
var t$1022;
if ((0 <= v$275)?(v$275 <= 123456789):false) {0;
} else {throw basis$0General$1.exn$Size$58;
};
t$1022 = (Array(v$275));
return t$1023(t$1022,v$276,0);
};
basis$0Vector$1$3.tabulatev$277 = function(v$297,v$298){var fix$1024 = {};
fix$1024.$init = function(v$294,v$295,v$296){lab$init: while (true) {if (v$296 >= v$297) {return v$294;
} else {(v$294[v$296] = (v$295(v$296)),0);
var t$1025 = v$294;
var t$1026 = v$295;
var t$1027 = SmlPrims.chk_ovf_i32(v$296 + 1);
var v$294 = t$1025;
var v$295 = t$1026;
var v$296 = t$1027;
continue lab$init;
};
};
};
var init$282 = fix$1024.$init;
return init$282(Array(v$297),v$298,0);
};
basis$0Vector$1$3.vector$299 = function(a$302){var v$989 = a$302.length;
var fix$1028 = {};
fix$1028.$init = function(v$986,v$987,v$988){lab$init: while (true) {if (v$988 >= v$989) {return v$986;
} else {(v$986[v$988] = (v$987(v$988)),0);
var t$1029 = v$986;
var t$1030 = v$987;
var t$1031 = SmlPrims.chk_ovf_i32(v$988 + 1);
var v$986 = t$1029;
var v$987 = t$1030;
var v$988 = t$1031;
continue lab$init;
};
};
};
var init$984 = fix$1028.$init;
return init$984(Array(v$989),function(i$992){return a$302[i$992];
},0);
};
basis$0Vector$1$3.updatev$306 = function(v$321,v$322,v$323){var v$914 = v$321.length;
if ((0 <= v$322)?(v$322 < v$914):false) {0;
} else {throw basis$0General$1.exn$Subscript$56;
};
return basis$0Vector$1$3.tabulate$255(v$321.length,function(j$316){return (v$322 == j$316)?v$323:v$321[j$316];
});
};
basis$0Vector$1$3.copy$324 = function(v$351,v$350,v$349){var n$330 = v$349.length;
if ((v$351 < 0)?true:((SmlPrims.chk_ovf_i32(v$351 + n$330)) > v$350.length)) {throw basis$0General$1.exn$Subscript$56;
} else {var fix$1032 = {};
fix$1032.$hi2lo = function(j$338){lab$hi2lo: while (true) {if (j$338 >= 0) {(v$350[SmlPrims.chk_ovf_i32(v$351 + j$338)] = v$349[j$338],0);
var t$1033 = SmlPrims.chk_ovf_i32(j$338 - 1);
var j$338 = t$1033;
continue lab$hi2lo;
} else {return 0;
};
};
};
var hi2lo$335 = fix$1032.$hi2lo;
return hi2lo$335(SmlPrims.chk_ovf_i32(n$330 - 1));
};
};
basis$0Vector$1$3.copyVec$352 = function(v$379,v$378,v$377){var n$358 = v$377.length;
if ((v$379 < 0)?true:((SmlPrims.chk_ovf_i32(v$379 + n$358)) > v$378.length)) {throw basis$0General$1.exn$Subscript$56;
} else {var fix$1034 = {};
fix$1034.$lo2hi = function(j$366){lab$lo2hi: while (true) {if (j$366 < n$358) {(v$378[SmlPrims.chk_ovf_i32(v$379 + j$366)] = v$377[j$366],0);
var t$1035 = SmlPrims.chk_ovf_i32(j$366 + 1);
var j$366 = t$1035;
continue lab$lo2hi;
} else {return 0;
};
};
};
var lo2hi$363 = fix$1034.$lo2hi;
return lo2hi$363(0);
};
};
basis$0Vector$1$3.app$380 = function(f$383,a$386){var n$387 = a$386.length;
var fix$1036 = {};
fix$1036.$lr = function(j$391){lab$lr: while (true) {if (j$391 < n$387) {f$383(a$386[j$391]);
var t$1037 = SmlPrims.chk_ovf_i32(j$391 + 1);
var j$391 = t$1037;
continue lab$lr;
} else {return 0;
};
};
};
var lr$388 = fix$1036.$lr;
return lr$388(0);
};
basis$0Vector$1$3.foldli$398 = function(f$401,e$404,a$407){var stop$408 = a$407.length;
var fix$1038 = {};
fix$1038.$lr = function(j$412,res$415){lab$lr: while (true) {if (j$412 < stop$408) {var t$1039 = SmlPrims.chk_ovf_i32(j$412 + 1);
var t$1040 = f$401([j$412,a$407[j$412],res$415]);
var j$412 = t$1039;
var res$415 = t$1040;
continue lab$lr;
} else {return res$415;
};
};
};
var lr$409 = fix$1038.$lr;
return lr$409(0,e$404);
};
basis$0Vector$1$3.foldri$420 = function(f$423,e$426,a$429){var fix$1041 = {};
fix$1041.$rl = function(j$433,res$436){lab$rl: while (true) {if (j$433 >= 0) {var t$1042 = SmlPrims.chk_ovf_i32(j$433 - 1);
var t$1043 = f$423([j$433,a$429[j$433],res$436]);
var j$433 = t$1042;
var res$436 = t$1043;
continue lab$rl;
} else {return res$436;
};
};
};
var rl$430 = fix$1041.$rl;
return rl$430(SmlPrims.chk_ovf_i32(a$429.length - 1),e$426);
};
basis$0Vector$1$3.appi$441 = function(f$444,a$447){var stop$448 = a$447.length;
var fix$1044 = {};
fix$1044.$lr = function(j$452){lab$lr: while (true) {if (j$452 < stop$448) {f$444([j$452,a$447[j$452]]);
var t$1045 = SmlPrims.chk_ovf_i32(j$452 + 1);
var j$452 = t$1045;
continue lab$lr;
} else {return 0;
};
};
};
var lr$449 = fix$1044.$lr;
return lr$449(0);
};
basis$0Vector$1$3.foldl$480 = function(f$483,e$486,a$489){var n$490 = a$489.length;
var fix$1046 = {};
fix$1046.$lr = function(v$500,v$501){lab$lr: while (true) {if (v$500 < n$490) {var t$1047 = SmlPrims.chk_ovf_i32(v$500 + 1);
var t$1048 = f$483([a$489[v$500],v$501]);
var v$500 = t$1047;
var v$501 = t$1048;
continue lab$lr;
} else {return v$501;
};
};
};
var lr$491 = fix$1046.$lr;
return lr$491(0,e$486);
};
basis$0Vector$1$3.foldr$502 = function(f$505,e$508,a$511){var n$512 = a$511.length;
var fix$1049 = {};
fix$1049.$rl = function(v$522,v$523){lab$rl: while (true) {if (v$522 >= 0) {var t$1050 = SmlPrims.chk_ovf_i32(v$522 - 1);
var t$1051 = f$505([a$511[v$522],v$523]);
var v$522 = t$1050;
var v$523 = t$1051;
continue lab$rl;
} else {return v$523;
};
};
};
var rl$513 = fix$1049.$rl;
return rl$513(SmlPrims.chk_ovf_i32(n$512 - 1),e$508);
};
basis$0Vector$1$3.modifyi$524 = function(f$527,a$530){var stop$531 = a$530.length;
var fix$1052 = {};
fix$1052.$lr = function(j$535){lab$lr: while (true) {if (j$535 < stop$531) {(a$530[j$535] = (f$527([j$535,a$530[j$535]])),0);
var t$1053 = SmlPrims.chk_ovf_i32(j$535 + 1);
var j$535 = t$1053;
continue lab$lr;
} else {return 0;
};
};
};
var lr$532 = fix$1052.$lr;
return lr$532(0);
};
basis$0Vector$1$3.modify$542 = function(f$545,a$548){var n$549 = a$548.length;
var fix$1054 = {};
fix$1054.$lr = function(j$553){lab$lr: while (true) {if (j$553 < n$549) {(a$548[j$553] = (f$545(a$548[j$553])),0);
var t$1055 = SmlPrims.chk_ovf_i32(j$553 + 1);
var j$553 = t$1055;
continue lab$lr;
} else {return 0;
};
};
};
var lr$550 = fix$1054.$lr;
return lr$550(0);
};
basis$0Vector$1$3.map$560 = function(f$563,a$566){var n$567 = a$566.length;
var b$568;
if ((0 <= n$567)?(n$567 <= 123456789):false) {0;
} else {throw basis$0General$1.exn$Size$58;
};
b$568 = (Array(n$567));
var fix$1056 = {};
fix$1056.$lr = function(j$572){lab$lr: while (true) {if (j$572 < n$567) {(b$568[j$572] = (f$563(a$566[j$572])),0);
var t$1057 = SmlPrims.chk_ovf_i32(j$572 + 1);
var j$572 = t$1057;
continue lab$lr;
} else {return b$568;
};
};
};
var lr$569 = fix$1056.$lr;
return lr$569(0);
};
basis$0Vector$1$3.mapi$579 = function(f$582,a$585){var stop$586 = a$585.length;
var newvec$587 = Array(stop$586);
var fix$1058 = {};
fix$1058.$lr = function(j$591){lab$lr: while (true) {if (j$591 < stop$586) {(newvec$587[j$591] = (f$582([j$591,a$585[j$591]])),0);
var t$1059 = SmlPrims.chk_ovf_i32(j$591 + 1);
var j$591 = t$1059;
continue lab$lr;
} else {return 0;
};
};
};
var lr$588 = fix$1058.$lr;
lr$588(0);
return newvec$587;
};
basis$0Vector$1$3.concat$600 = function(vecs$603){var fix$1060 = {};
fix$1060.$total_length = function(v$616,v$1007){lab$total_length: while (true) {if (v$616 == null) {return v$1007;
} else {var v$618 = v$616;
var v$619 = v$618[0];
var v$620 = v$618[1];
var t$1061 = v$620;
var t$1062 = SmlPrims.chk_ovf_i32(v$619.length + v$1007);
var v$616 = t$1061;
var v$1007 = t$1062;
continue lab$total_length;
};
};
};
var total_length$604 = fix$1060.$total_length;
var n$622 = total_length$604(vecs$603,0);
var v$623;
if ((0 <= n$622)?(n$622 <= 123456789):false) {0;
} else {throw basis$0General$1.exn$Size$58;
};
v$623 = (Array(n$622));
var fix$1063 = {};
fix$1063.$copyall = function(v$1008,v$636){lab$copyall: while (true) {if (v$636 == null) {return v$623;
} else {var v$652 = v$636;
var v$653 = v$652[0];
var v$654 = v$652[1];
var x_n$638 = v$653.length;
var fix$1064 = {};
fix$1064.$copy = function(j$642){lab$copy: while (true) {if (j$642 < x_n$638) {(v$623[SmlPrims.chk_ovf_i32(v$1008 + j$642)] = v$653[j$642],0);
var t$1065 = SmlPrims.chk_ovf_i32(j$642 + 1);
var j$642 = t$1065;
continue lab$copy;
} else {return 0;
};
};
};
var copy$639 = fix$1064.$copy;
copy$639(0);
var t$1066 = SmlPrims.chk_ovf_i32(v$1008 + x_n$638);
var t$1067 = v$654;
var v$1008 = t$1066;
var v$636 = t$1067;
continue lab$copyall;
};
};
};
var copyall$624 = fix$1063.$copyall;
return copyall$624(0,vecs$603);
};
basis$0Vector$1$3.findi$655 = function(p$658,a$661){var stop$662 = a$661.length;
var fix$1068 = {};
fix$1068.$lr = function(j$666){lab$lr: while (true) {if (j$666 < stop$662) {if (p$658([j$666,a$661[j$666]])) {return [0,[j$666,a$661[j$666]]];
} else {var t$1069 = SmlPrims.chk_ovf_i32(j$666 + 1);
var j$666 = t$1069;
continue lab$lr;
};
} else {return [1];
};
};
};
var lr$663 = fix$1068.$lr;
return lr$663(0);
};
basis$0Vector$1$3.find$675 = function(p$678,a$681){var stop$682 = a$681.length;
var fix$1070 = {};
fix$1070.$lr = function(j$686){lab$lr: while (true) {if (j$686 < stop$682) {if (p$678(a$681[j$686])) {return [0,a$681[j$686]];
} else {var t$1071 = SmlPrims.chk_ovf_i32(j$686 + 1);
var j$686 = t$1071;
continue lab$lr;
};
} else {return [1];
};
};
};
var lr$683 = fix$1070.$lr;
return lr$683(0);
};
basis$0Vector$1$3.exists$695 = function(p$698,a$701){var stop$702 = a$701.length;
var fix$1072 = {};
fix$1072.$lr = function(j$706){lab$lr: while (true) {if (j$706 < stop$702) {if (p$698(a$701[j$706])) {return true;
} else {var t$1073 = SmlPrims.chk_ovf_i32(j$706 + 1);
var j$706 = t$1073;
continue lab$lr;
};
} else {return false;
};
};
};
var lr$703 = fix$1072.$lr;
return lr$703(0);
};
basis$0Vector$1$3.all$715 = function(p$718,a$721){var stop$722 = a$721.length;
var fix$1074 = {};
fix$1074.$lr = function(j$726){lab$lr: while (true) {if (j$726 >= stop$722) {return true;
} else {if (p$718(a$721[j$726])) {var t$1075 = SmlPrims.chk_ovf_i32(j$726 + 1);
var j$726 = t$1075;
continue lab$lr;
} else {return false;
};
};
};
};
var lr$723 = fix$1074.$lr;
return lr$723(0);
};
basis$0Vector$1$3.collate$735 = function(cmp$738,v$742){var v$771 = v$742[0];
var v$772 = v$742[1];
var n1$743 = v$771.length;
var n2$744 = v$772.length;
var stop$745 = (n1$743 < n2$744)?n1$743:n2$744;
var fix$1076 = {};
fix$1076.$h = function(j$753){lab$h: while (true) {if (j$753 == stop$745) {return (n1$743 < n2$744)?0:((n1$743 > n2$744)?1:2);
} else {var v$770 = cmp$738([v$771[j$753],v$772[j$753]]);
switch (v$770) { case 2: {var t$1077 = SmlPrims.chk_ovf_i32(j$753 + 1);
var j$753 = t$1077;
continue lab$h;
 break; }default: {return v$770;
} };
};
};
};
var h$750 = fix$1076.$h;
return h$750(0);
};
return 0;
})();