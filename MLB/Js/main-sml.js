if ((typeof(main$0main$1)) == "undefined") {main$0main$1 = {};
};
(function(){main$0main$1.body$50;
var v$55 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$84,"body");
switch (v$55[0]) { case 0: {main$0main$1.body$50 = v$55[1];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"cannot find body element"];
} };
var fix$163 = {};
fix$163.$connect = function(v$61){if (v$61 == null) {return document.createTextNode("");
} else {var v$71 = v$61;
var v$72 = v$71[1];
if (v$72 == null) {return v$71[0];
} else {var v$74 = v$71[0];
var v$160 = fix$163.$connect(v$72);
var e$114 = document.createDocumentFragment();
(function(e,a){return e.appendChild(a);})(e$114,v$74);
(function(e,a){return e.appendChild(a);})(e$114,v$160);
return e$114;
};
};
};
main$0main$1.connect$58 = fix$163.$connect;
var links$77 = main$0Data$1.getReports$266(0);
var hrefs$78 = basis$0List$1.map$693(function(s$81){var attrs$116 = null;
var elem$117;
var attrs$125 = [["href",s$81],null];
var elem$126 = document.createTextNode(s$81);
var newelem$127 = document.createElement("a");
var fix$164 = {};
fix$164.$app = function(v$129){lab$app: while (true) {if (v$129 == null) {return 0;
} else {var v$130 = v$129;
var v$131 = v$130[0];
var v$132 = v$130[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$127,v$131[0],v$131[1]);
var t$165 = v$132;
var v$129 = t$165;
continue lab$app;
};
};
};
var app$128 = fix$164.$app;
app$128(attrs$125);
(function(e,a){return e.appendChild(a);})(newelem$127,elem$126);
elem$117 = newelem$127;
var newelem$118 = document.createElement("li");
var fix$166 = {};
fix$166.$app = function(v$120){lab$app: while (true) {if (v$120 == null) {return 0;
} else {var v$121 = v$120;
var v$122 = v$121[0];
var v$123 = v$121[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$118,v$122[0],v$122[1]);
var t$167 = v$123;
var v$120 = t$167;
continue lab$app;
};
};
};
var app$119 = fix$166.$app;
app$119(attrs$116);
(function(e,a){return e.appendChild(a);})(newelem$118,elem$117);
return newelem$118;
},links$77);
var t$173 = main$0main$1.body$50;
var t$168;
var v$161;
var attrs$141 = null;
var elem$142 = document.createTextNode("Hi there");
var newelem$143 = document.createElement("h2");
var fix$169 = {};
fix$169.$app = function(v$145){lab$app: while (true) {if (v$145 == null) {return 0;
} else {var v$146 = v$145;
var v$147 = v$146[0];
var v$148 = v$146[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$143,v$147[0],v$147[1]);
var t$170 = v$148;
var v$145 = t$170;
continue lab$app;
};
};
};
var app$144 = fix$169.$app;
app$144(attrs$141);
(function(e,a){return e.appendChild(a);})(newelem$143,elem$142);
v$161 = newelem$143;
var v$162;
var attrs$151 = null;
var elem$152 = main$0main$1.connect$58(hrefs$78);
var newelem$153 = document.createElement("ul");
var fix$171 = {};
fix$171.$app = function(v$155){lab$app: while (true) {if (v$155 == null) {return 0;
} else {var v$156 = v$155;
var v$157 = v$156[0];
var v$158 = v$156[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$153,v$157[0],v$157[1]);
var t$172 = v$158;
var v$155 = t$172;
continue lab$app;
};
};
};
var app$154 = fix$171.$app;
app$154(attrs$151);
(function(e,a){return e.appendChild(a);})(newelem$153,elem$152);
v$162 = newelem$153;
var e$139 = document.createDocumentFragment();
(function(e,a){return e.appendChild(a);})(e$139,v$161);
(function(e,a){return e.appendChild(a);})(e$139,v$162);
t$168 = e$139;
(function(e,a){return e.appendChild(a);})(t$173,t$168);
return 0;
})();
