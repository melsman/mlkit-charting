if ((typeof(basis$0Option$1)) == "undefined") {basis$0Option$1 = {};
};
(function(){basis$0Option$1.getOpt$51 = function(v$193){return basis$0General$1.getOpt$105(v$193[0],v$193[1]);
};
basis$0Option$1.isSome$52 = function(v$194){return function(v$196){return basis$0General$1.isSome$120(v$196);
};
};
basis$0Option$1.valOf$53 = function(v$195){return function(v$197){return basis$0General$1.valOf$128(v$197);
};
};
basis$0Option$1.filter$54 = function(p$57,x$60){return (p$57(x$60))?[0,x$60]:[1];
};
basis$0Option$1.map$65 = function(v$68,v$71){switch (v$71[0]) { case 1: {return [1];
 break; }default: {var v$82 = v$71[1];
return [0,v$68(v$82)];
} };
};
basis$0Option$1.app$83 = function(v$86,v$89){switch (v$89[0]) { case 1: {return 0;
 break; }default: {var v$100 = v$89[1];
return v$86(v$100);
} };
};
basis$0Option$1.join$101 = function(v$104){switch (v$104[0]) { case 1: {return [1];
 break; }default: {return v$104[1];
} };
};
basis$0Option$1.mapPartial$111 = function(v$114,v$117){switch (v$117[0]) { case 1: {return [1];
 break; }default: {var v$128 = v$117[1];
return v$114(v$128);
} };
};
basis$0Option$1.compose$129 = function(v$143,v$144){return function(x$136){var v$141 = v$144(x$136);
switch (v$141[0]) { case 1: {return [1];
 break; }default: {var v$142 = v$141[1];
return [0,v$143(v$142)];
} };
};
};
basis$0Option$1.composePartial$145 = function(v$159,v$160){return function(x$152){var v$157 = v$160(x$152);
switch (v$157[0]) { case 1: {return [1];
 break; }default: {var v$158 = v$157[1];
return v$159(v$158);
} };
};
};
return 0;
})();
