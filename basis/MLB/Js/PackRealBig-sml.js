if ((typeof(basis$0PackRealBig$1)) == "undefined") {basis$0PackRealBig$1 = {};
};
(function(){basis$0PackRealBig$1.bytesPerElem$51 = 8;
basis$0PackRealBig$1.isBigEndian$52 = true;
basis$0PackRealBig$1.toBytes$61 = function(r$64){var v$125 = SmlPrims.real_to_bytes(r$64);
var len$126 = v$125.length;
return basis$0ByteTable$1$16.tabulate$2470(len$126,function(i$127){return basis$0ByteTable$1$16.sub$2455(v$125,SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(len$126 - 1)) - i$127));
});
};
basis$0PackRealBig$1.fromBytes$65 = function(v$68){var t$148;
var len$131 = v$68.length;
t$148 = (basis$0ByteTable$1$16.tabulate$2470(len$131,function(i$132){return basis$0ByteTable$1$16.sub$2455(v$68,SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(len$131 - 1)) - i$132));
}));
return SmlPrims.bytes_to_real(t$148);
};
basis$0PackRealBig$1.subVec$69 = function(v$74,v$75){var t$150 = basis$0PackRealLittle$1.subVec$69;
var t$149;
var len$134 = v$74.length;
t$149 = (basis$0ByteTable$1$16.tabulate$2470(len$134,function(i$135){return basis$0ByteTable$1$16.sub$2455(v$74,SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(len$134 - 1)) - i$135));
}));
return t$150(t$149,v$75);
};
basis$0PackRealBig$1.subArr$76 = function(v$84,v$85){var v$146 = basis$0ByteTable$1$16.tabulate$2470(8,function(j$83){return basis$0ByteTable$1$22.sub$3573(v$84,SmlPrims.chk_ovf_i32(j$83 + (SmlPrims.chk_ovf_i32(v$85 * 8))));
});
var t$152 = basis$0PackRealLittle$1.subVec$69;
var t$151;
var len$140 = v$146.length;
t$151 = (basis$0ByteTable$1$16.tabulate$2470(len$140,function(i$141){return basis$0ByteTable$1$16.sub$2455(v$146,SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(len$140 - 1)) - i$141));
}));
return t$152(t$151,8);
};
basis$0PackRealBig$1.update$86 = function(v$95,v$96,v$97){var t$156 = basis$0ByteTable$1$22.copyVec$4015;
var t$155 = v$96;
var t$154 = v$95;
var t$153;
var v$143 = SmlPrims.real_to_bytes(v$97);
var len$144 = v$143.length;
t$153 = (basis$0ByteTable$1$16.tabulate$2470(len$144,function(i$145){return basis$0ByteTable$1$16.sub$2455(v$143,SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(len$144 - 1)) - i$145));
}));
return t$156(t$155,t$154,t$153);
};
return 0;
})();