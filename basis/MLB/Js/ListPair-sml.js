if ((typeof(basis$0ListPair$1)) == "undefined") {basis$0ListPair$1 = {};
};
(function(){basis$0ListPair$1.en$UnequalLengths$50 = new String("UnequalLengths");
basis$0ListPair$1.exn$UnequalLengths$50 = [basis$0ListPair$1.en$UnequalLengths$50];
basis$0ListPair$1.zip$51 = function(v$90,v$91){var fix$737 = {};
fix$737.$h = function(v$59,v$62,v$65){lab$h: while (true) {if (v$59 == null) {return basis$0List$1.rev$253(v$65);
} else {if (v$62 == null) {return basis$0List$1.rev$253(v$65);
} else {var v$81 = v$59;
var v$82 = v$81[0];
var v$83 = v$81[1];
var v$84 = v$62;
var v$85 = v$84[0];
var v$86 = v$84[1];
var t$738 = v$83;
var t$739 = v$86;
var t$740 = [[v$82,v$85],v$65];
var v$59 = t$738;
var v$62 = t$739;
var v$65 = t$740;
continue lab$h;
};
};
};
};
var h$56 = fix$737.$h;
return h$56(v$90,v$91,null);
};
basis$0ListPair$1.zipEq$92 = function(v$134,v$135){var fix$741 = {};
fix$741.$h = function(v$100,v$103,v$106){lab$h: while (true) {if (v$100 == null) {if (v$103 == null) {return basis$0List$1.rev$253(v$106);
} else {throw basis$0ListPair$1.exn$UnequalLengths$50;
};
} else {if (v$103 == null) {throw basis$0ListPair$1.exn$UnequalLengths$50;
} else {var v$124 = v$100;
var v$125 = v$124[0];
var v$126 = v$124[1];
var v$127 = v$103;
var v$128 = v$127[0];
var v$129 = v$127[1];
var t$742 = v$126;
var t$743 = v$129;
var t$744 = [[v$125,v$128],v$106];
var v$100 = t$742;
var v$103 = t$743;
var v$106 = t$744;
continue lab$h;
};
};
};
};
var h$97 = fix$741.$h;
return h$97(v$134,v$135,null);
};
basis$0ListPair$1.unzip$136 = function(xys$139){var fix$745 = {};
fix$745.$h = function(v$155,v$735,v$736){lab$h: while (true) {if (v$155 == null) {return [basis$0List$1.rev$253(v$735),basis$0List$1.rev$253(v$736)];
} else {var v$158 = v$155;
var v$159 = v$158[0];
var v$160 = v$159[0];
var v$161 = v$159[1];
var v$162 = v$158[1];
var t$746 = v$162;
var t$747 = [v$160,v$735];
var t$748 = [v$161,v$736];
var v$155 = t$746;
var v$735 = t$747;
var v$736 = t$748;
continue lab$h;
};
};
};
var h$140 = fix$745.$h;
return h$140(xys$139,null,null);
};
basis$0ListPair$1.map$165 = function(f$168,v$172){var v$207 = v$172[0];
var v$208 = v$172[1];
var fix$749 = {};
fix$749.$h = function(v$176,v$179,v$182){lab$h: while (true) {if (v$176 == null) {return basis$0List$1.rev$253(v$182);
} else {if (v$179 == null) {return basis$0List$1.rev$253(v$182);
} else {var v$198 = v$176;
var v$199 = v$198[0];
var v$200 = v$198[1];
var v$201 = v$179;
var v$202 = v$201[0];
var v$203 = v$201[1];
var t$750 = v$200;
var t$751 = v$203;
var t$752 = [f$168([v$199,v$202]),v$182];
var v$176 = t$750;
var v$179 = t$751;
var v$182 = t$752;
continue lab$h;
};
};
};
};
var h$173 = fix$749.$h;
return h$173(v$207,v$208,null);
};
basis$0ListPair$1.mapEq$209 = function(f$212,v$216){var v$256 = v$216[0];
var v$257 = v$216[1];
var fix$753 = {};
fix$753.$h = function(v$220,v$223,v$226){lab$h: while (true) {if (v$220 == null) {if (v$223 == null) {return basis$0List$1.rev$253(v$226);
} else {throw basis$0ListPair$1.exn$UnequalLengths$50;
};
} else {if (v$223 == null) {throw basis$0ListPair$1.exn$UnequalLengths$50;
} else {var v$245 = v$220;
var v$246 = v$245[0];
var v$247 = v$245[1];
var v$248 = v$223;
var v$249 = v$248[0];
var v$250 = v$248[1];
var t$754 = v$247;
var t$755 = v$250;
var t$756 = [f$212([v$246,v$249]),v$226];
var v$220 = t$754;
var v$223 = t$755;
var v$226 = t$756;
continue lab$h;
};
};
};
};
var h$217 = fix$753.$h;
return h$217(v$256,v$257,null);
};
basis$0ListPair$1.app$258 = function(f$261,v$265){var v$295 = v$265[0];
var v$296 = v$265[1];
var fix$757 = {};
fix$757.$h = function(v$269,v$272){lab$h: while (true) {if (v$269 == null) {return 0;
} else {if (v$272 == null) {return 0;
} else {var v$288 = v$269;
var v$289 = v$288[0];
var v$290 = v$288[1];
var v$291 = v$272;
var v$292 = v$291[0];
var v$293 = v$291[1];
f$261([v$289,v$292]);
var t$758 = v$290;
var t$759 = v$293;
var v$269 = t$758;
var v$272 = t$759;
continue lab$h;
};
};
};
};
var h$266 = fix$757.$h;
return h$266(v$295,v$296);
};
basis$0ListPair$1.appEq$297 = function(f$300,v$304){var v$337 = v$304[0];
var v$338 = v$304[1];
var fix$760 = {};
fix$760.$h = function(v$308,v$311){lab$h: while (true) {if (v$308 == null) {if (v$311 == null) {return 0;
} else {throw basis$0ListPair$1.exn$UnequalLengths$50;
};
} else {if (v$311 == null) {throw basis$0ListPair$1.exn$UnequalLengths$50;
} else {var v$329 = v$308;
var v$330 = v$329[0];
var v$331 = v$329[1];
var v$332 = v$311;
var v$333 = v$332[0];
var v$334 = v$332[1];
f$300([v$330,v$333]);
var t$761 = v$331;
var t$762 = v$334;
var v$308 = t$761;
var v$311 = t$762;
continue lab$h;
};
};
};
};
var h$305 = fix$760.$h;
return h$305(v$337,v$338);
};
basis$0ListPair$1.all$339 = function(p$342,v$346){var v$378 = v$346[0];
var v$379 = v$346[1];
var fix$763 = {};
fix$763.$h = function(v$350,v$353){lab$h: while (true) {if (v$350 == null) {return true;
} else {if (v$353 == null) {return true;
} else {var v$371 = v$350;
var v$372 = v$371[0];
var v$373 = v$371[1];
var v$374 = v$353;
var v$375 = v$374[0];
var v$376 = v$374[1];
if (p$342([v$372,v$375])) {var t$764 = v$373;
var t$765 = v$376;
var v$350 = t$764;
var v$353 = t$765;
continue lab$h;
} else {return false;
};
};
};
};
};
var h$347 = fix$763.$h;
return h$347(v$378,v$379);
};
basis$0ListPair$1.allEq$380 = function(p$383,v$387){var v$422 = v$387[0];
var v$423 = v$387[1];
var fix$766 = {};
fix$766.$h = function(v$391,v$394){lab$h: while (true) {if (v$391 == null) {return (v$394 == null)?true:false;
} else {if (v$394 == null) {return false;
} else {var v$414 = v$391;
var v$415 = v$414[0];
var v$416 = v$414[1];
var v$417 = v$394;
var v$418 = v$417[0];
var v$419 = v$417[1];
if (p$383([v$415,v$418])) {var t$767 = v$416;
var t$768 = v$419;
var v$391 = t$767;
var v$394 = t$768;
continue lab$h;
} else {return false;
};
};
};
};
};
var h$388 = fix$766.$h;
return h$388(v$422,v$423);
};
basis$0ListPair$1.exists$424 = function(p$427,v$431){var v$463 = v$431[0];
var v$464 = v$431[1];
var fix$769 = {};
fix$769.$h = function(v$435,v$438){lab$h: while (true) {if (v$435 == null) {return false;
} else {if (v$438 == null) {return false;
} else {var v$456 = v$435;
var v$457 = v$456[0];
var v$458 = v$456[1];
var v$459 = v$438;
var v$460 = v$459[0];
var v$461 = v$459[1];
if (p$427([v$457,v$460])) {return true;
} else {var t$770 = v$458;
var t$771 = v$461;
var v$435 = t$770;
var v$438 = t$771;
continue lab$h;
};
};
};
};
};
var h$432 = fix$769.$h;
return h$432(v$463,v$464);
};
basis$0ListPair$1.foldr$465 = function(f$468,e$471,v$475){var v$503 = v$475[0];
var v$504 = v$475[1];
var fix$772 = {};
fix$772.$h = function(v$479,v$482){if (v$479 == null) {return e$471;
} else {if (v$482 == null) {return e$471;
} else {var v$496 = v$479;
var v$497 = v$496[0];
var v$498 = v$496[1];
var v$499 = v$482;
var v$500 = v$499[0];
var v$501 = v$499[1];
return f$468([v$497,v$500,fix$772.$h(v$498,v$501)]);
};
};
};
var h$476 = fix$772.$h;
return h$476(v$503,v$504);
};
basis$0ListPair$1.foldrEq$505 = function(f$508,e$511,v$515){var v$546 = v$515[0];
var v$547 = v$515[1];
var fix$773 = {};
fix$773.$h = function(v$519,v$522){if (v$519 == null) {if (v$522 == null) {return e$511;
} else {throw basis$0ListPair$1.exn$UnequalLengths$50;
};
} else {if (v$522 == null) {throw basis$0ListPair$1.exn$UnequalLengths$50;
} else {var v$538 = v$519;
var v$539 = v$538[0];
var v$540 = v$538[1];
var v$541 = v$522;
var v$542 = v$541[0];
var v$543 = v$541[1];
return f$508([v$539,v$542,fix$773.$h(v$540,v$543)]);
};
};
};
var h$516 = fix$773.$h;
return h$516(v$546,v$547);
};
basis$0ListPair$1.foldl$548 = function(f$551,e$554,v$558){var v$593 = v$558[0];
var v$594 = v$558[1];
var fix$774 = {};
fix$774.$h = function(v$562,v$565,v$568){lab$h: while (true) {if (v$565 == null) {return v$562;
} else {if (v$568 == null) {return v$562;
} else {var v$585 = v$565;
var v$586 = v$585[0];
var v$587 = v$585[1];
var v$588 = v$568;
var v$589 = v$588[0];
var v$590 = v$588[1];
var t$775 = f$551([v$586,v$589,v$562]);
var t$776 = v$587;
var t$777 = v$590;
var v$562 = t$775;
var v$565 = t$776;
var v$568 = t$777;
continue lab$h;
};
};
};
};
var h$559 = fix$774.$h;
return h$559(e$554,v$593,v$594);
};
basis$0ListPair$1.foldlEq$595 = function(f$598,e$601,v$605){var v$645 = v$605[0];
var v$646 = v$605[1];
var fix$778 = {};
fix$778.$h = function(v$609,v$612,v$615){lab$h: while (true) {if (v$612 == null) {if (v$615 == null) {return v$609;
} else {throw basis$0ListPair$1.exn$UnequalLengths$50;
};
} else {if (v$615 == null) {throw basis$0ListPair$1.exn$UnequalLengths$50;
} else {var v$635 = v$612;
var v$636 = v$635[0];
var v$637 = v$635[1];
var v$638 = v$615;
var v$639 = v$638[0];
var v$640 = v$638[1];
var t$779 = f$598([v$636,v$639,v$609]);
var t$780 = v$637;
var t$781 = v$640;
var v$609 = t$779;
var v$612 = t$780;
var v$615 = t$781;
continue lab$h;
};
};
};
};
var h$606 = fix$778.$h;
return h$606(e$601,v$645,v$646);
};
return 0;
})();
