if ((typeof(main$0main$1)) == "undefined") {main$0main$1 = {};
};
(function(){main$0main$1.body$50;
var v$55 = (function(d,id){return SmlPrims.option(d.getElementById(id));})(js$0Js$1.document$84,"body");
switch (v$55[0]) { case 0: {main$0main$1.body$50 = v$55[1];
 break; }default: {throw [basis$0Initial$1.en$Fail$50,"cannot find body element"];
} };
var fix$179 = {};
fix$179.$connect = function(v$61){if (v$61 == null) {return document.createTextNode("");
} else {var v$71 = v$61;
var v$72 = v$71[1];
if (v$72 == null) {return v$71[0];
} else {var v$74 = v$71[0];
var v$176 = fix$179.$connect(v$72);
var e$120 = document.createDocumentFragment();
(function(e,a){return e.appendChild(a);})(e$120,v$74);
(function(e,a){return e.appendChild(a);})(e$120,v$176);
return e$120;
};
};
};
main$0main$1.connect$58 = fix$179.$connect;
main$0main$1.list_elem$75;
var attrs$122 = null;
var newelem$123 = document.createElement("div");
var fix$180 = {};
fix$180.$app = function(v$125){lab$app: while (true) {if (v$125 == null) {return 0;
} else {var v$126 = v$125;
var v$127 = v$126[0];
var v$128 = v$126[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$123,v$127[0],v$127[1]);
var t$181 = v$128;
var v$125 = t$181;
continue lab$app;
};
};
};
var app$124 = fix$180.$app;
app$124(attrs$122);
main$0main$1.list_elem$75 = newelem$123;
var t$185 = main$0main$1.body$50;
var t$182;
var v$177;
var attrs$136 = null;
var elem$137 = document.createTextNode("Hi there");
var newelem$138 = document.createElement("h2");
var fix$183 = {};
fix$183.$app = function(v$140){lab$app: while (true) {if (v$140 == null) {return 0;
} else {var v$141 = v$140;
var v$142 = v$141[0];
var v$143 = v$141[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$138,v$142[0],v$142[1]);
var t$184 = v$143;
var v$140 = t$184;
continue lab$app;
};
};
};
var app$139 = fix$183.$app;
app$139(attrs$136);
(function(e,a){return e.appendChild(a);})(newelem$138,elem$137);
v$177 = newelem$138;
var e$134 = document.createDocumentFragment();
(function(e,a){return e.appendChild(a);})(e$134,v$177);
(function(e,a){return e.appendChild(a);})(e$134,main$0main$1.list_elem$75);
t$182 = e$134;
(function(e,a){return e.appendChild(a);})(t$185,t$182);
main$0Data$1.getReports$275(function(links$81){var hrefs$82 = basis$0List$1.map$693(function(s$85){var attrs$146 = null;
var elem$147;
var attrs$155 = [["href",s$85],null];
var elem$156 = document.createTextNode(s$85);
var newelem$157 = document.createElement("a");
var fix$186 = {};
fix$186.$app = function(v$159){lab$app: while (true) {if (v$159 == null) {return 0;
} else {var v$160 = v$159;
var v$161 = v$160[0];
var v$162 = v$160[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$157,v$161[0],v$161[1]);
var t$187 = v$162;
var v$159 = t$187;
continue lab$app;
};
};
};
var app$158 = fix$186.$app;
app$158(attrs$155);
(function(e,a){return e.appendChild(a);})(newelem$157,elem$156);
elem$147 = newelem$157;
var newelem$148 = document.createElement("li");
var fix$188 = {};
fix$188.$app = function(v$150){lab$app: while (true) {if (v$150 == null) {return 0;
} else {var v$151 = v$150;
var v$152 = v$151[0];
var v$153 = v$151[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$148,v$152[0],v$152[1]);
var t$189 = v$153;
var v$150 = t$189;
continue lab$app;
};
};
};
var app$149 = fix$188.$app;
app$149(attrs$146);
(function(e,a){return e.appendChild(a);})(newelem$148,elem$147);
return newelem$148;
},links$81);
var t$193 = main$0main$1.list_elem$75;
var t$190;
var attrs$165 = null;
var elem$166 = main$0main$1.connect$58(hrefs$82);
var newelem$167 = document.createElement("ul");
var fix$191 = {};
fix$191.$app = function(v$169){lab$app: while (true) {if (v$169 == null) {return 0;
} else {var v$170 = v$169;
var v$171 = v$170[0];
var v$172 = v$170[1];
(function(e,a,b){return e.setAttribute(a,b);})(newelem$167,v$171[0],v$171[1]);
var t$192 = v$172;
var v$169 = t$192;
continue lab$app;
};
};
};
var app$168 = fix$191.$app;
app$168(attrs$165);
(function(e,a){return e.appendChild(a);})(newelem$167,elem$166);
t$190 = newelem$167;
return (function(e,a){return e.appendChild(a);})(t$193,t$190);
});
return 0;
})();
