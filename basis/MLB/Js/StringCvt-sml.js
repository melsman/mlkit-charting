if ((typeof(basis$0StringCvt$1)) == "undefined") {basis$0StringCvt$1 = {};
};
(function(){basis$0StringCvt$1.eq_radix$302 = function(v$304,v$305){switch (v$304) { case 0: {switch (v$305) { case 0: {return true;
 break; }default: {return false;
} };
 break; }case 1: {switch (v$305) { case 1: {return true;
 break; }default: {return false;
} };
 break; }case 2: {switch (v$305) { case 2: {return true;
 break; }default: {return false;
} };
 break; }case 3: {switch (v$305) { case 3: {return true;
 break; }default: {return false;
} };
 break; } };
};
basis$0StringCvt$1.eq_realfmt$306 = function(v$308,v$309){switch (v$308[0]) { case 0: {switch (v$309[0]) { case 0: {var v$310 = v$308[1];
var v$311 = v$309[1];
return basis$0General$1.eq_option$256(function(v$312){return v$312[0] == v$312[1];
},[v$310,v$311]);
 break; }default: {return false;
} };
 break; }case 1: {switch (v$309[0]) { case 1: {var v$313 = v$308[1];
var v$314 = v$309[1];
return basis$0General$1.eq_option$256(function(v$315){return v$315[0] == v$315[1];
},[v$313,v$314]);
 break; }default: {return false;
} };
 break; }case 2: {switch (v$309[0]) { case 2: {var v$316 = v$308[1];
var v$317 = v$309[1];
return basis$0General$1.eq_option$256(function(v$318){return v$318[0] == v$318[1];
},[v$316,v$317]);
 break; }default: {return false;
} };
 break; }case 3: {switch (v$309[0]) { case 3: {return true;
 break; }default: {return false;
} };
 break; } };
};
basis$0StringCvt$1.padLeft$111 = function(c$114,n$117,s$120){var ssize$121 = s$120.length;
var fix$358 = {};
fix$358.$f = function(v$125){switch (v$125) { case 0: {return null;
 break; }default: {return [c$114,fix$358.$f(SmlPrims.chk_ovf_i32(v$125 - 1))];
} };
};
var f$122 = fix$358.$f;
return (n$117 <= ssize$121)?s$120:(SmlPrims.concat([SmlPrims.implode(f$122(SmlPrims.chk_ovf_i32(n$117 - ssize$121))),[s$120,null]]));
};
basis$0StringCvt$1.padRight$135 = function(c$138,n$141,s$144){var ssize$145 = s$144.length;
var fix$359 = {};
fix$359.$f = function(v$149){switch (v$149) { case 0: {return null;
 break; }default: {return [c$138,fix$359.$f(SmlPrims.chk_ovf_i32(v$149 - 1))];
} };
};
var f$146 = fix$359.$f;
return (n$141 <= ssize$145)?s$144:(SmlPrims.concat([s$144,[SmlPrims.implode(f$146(SmlPrims.chk_ovf_i32(n$141 - ssize$145))),null]]));
};
basis$0StringCvt$1.scanString$159 = function(scan$162,s$165){var len$166 = s$165.length;
var v$179 = (scan$162(function(i$330){return (i$330 >= len$166)?[1]:[0,[basis$0ByteTable$1$5.sub$247(s$165,i$330),SmlPrims.chk_ovf_i32(i$330 + 1)]];
}))(0);
switch (v$179[0]) { case 1: {return [1];
 break; }default: {var v$180 = v$179[1];
var v$181 = v$180[0];
return [0,v$181];
} };
};
basis$0StringCvt$1.splitl$202 = function(p$205,getc$208,src$211){var fix$360 = {};
fix$360.$h = function(v$230,v$231){lab$h: while (true) {var v$222 = getc$208(v$231);
switch (v$222[0]) { case 1: {var t$364;
var t$363;
var fix$361 = {};
fix$361.$rev_rec = function(v$334){lab$rev_rec: while (true) {var v$335 = v$334[0];
if (v$335 == null) {return v$334;
} else {var v$336 = v$335;
var v$337 = v$336[0];
var v$338 = v$336[1];
var v$339 = v$334[1];
var t$362 = [v$338,[v$337,v$339]];
var v$334 = t$362;
continue lab$rev_rec;
};
};
};
var rev_rec$333 = fix$361.$rev_rec;
t$363 = (rev_rec$333([v$230,null]))[1];
t$364 = (SmlPrims.implode(t$363));
return [t$364,v$231];
 break; }default: {var v$227 = v$222[1];
var v$228 = v$227[0];
var v$229 = v$227[1];
if (p$205(v$228)) {var t$369 = [v$228,v$230];
var t$370 = v$229;
var v$230 = t$369;
var v$231 = t$370;
continue lab$h;
} else {var t$368;
var t$367;
var fix$365 = {};
fix$365.$rev_rec = function(v$343){lab$rev_rec: while (true) {var v$344 = v$343[0];
if (v$344 == null) {return v$343;
} else {var v$345 = v$344;
var v$346 = v$345[0];
var v$347 = v$345[1];
var v$348 = v$343[1];
var t$366 = [v$347,[v$346,v$348]];
var v$343 = t$366;
continue lab$rev_rec;
};
};
};
var rev_rec$342 = fix$365.$rev_rec;
t$367 = (rev_rec$342([v$230,null]))[1];
t$368 = (SmlPrims.implode(t$367));
return [t$368,v$231];
};
} };
};
};
var h$212 = fix$360.$h;
return h$212(null,src$211);
};
basis$0StringCvt$1.takel$232 = function(p$235,getc$238,src$241){return (basis$0StringCvt$1.splitl$202(p$235,getc$238,src$241))[0];
};
basis$0StringCvt$1.dropl$246 = function(p$249,f$252,s$255){return (basis$0StringCvt$1.splitl$202(p$249,f$252,s$255))[1];
};
basis$0StringCvt$1.skipWS$272 = function(getc$275,s$351){return (basis$0StringCvt$1.splitl$202(function(c$357){return (c$357 == 32)?true:((9 <= c$357)?(c$357 <= 13):false);
},getc$275,s$351))[1];
};
return 0;
})();