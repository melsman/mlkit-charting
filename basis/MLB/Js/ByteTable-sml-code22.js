if ((typeof(basis$0ByteTable$1$22)) == "undefined") {basis$0ByteTable$1$22 = {};
};
(function(){basis$0ByteTable$1$22.sub_array_unsafe$3519 = function(v$4422,v$4423){return v$4422[v$4423];
};
basis$0ByteTable$1$22.update_array_unsafe$3526 = function(v$4424,v$4425,v$4426){return (v$4424[v$4425] = v$4426,0);
};
basis$0ByteTable$1$22.alloc_array_unsafe$3535 = function(i$3538){return Array(i$3538);
};
basis$0ByteTable$1$22.length_array$3539 = function(a$3542){return a$3542.length;
};
basis$0ByteTable$1$22.sub_vector_unsafe$3543 = function(v$4427,v$4428){return v$4427.charCodeAt(v$4428);
};
basis$0ByteTable$1$22.fromList$3550 = function(es$4201){return SmlPrims.charsToCharArray(es$4201);
};
basis$0ByteTable$1$22.concat$3551 = function(vs$4202){return SmlPrims.charArraysConcat(vs$4202);
};
basis$0ByteTable$1$22.length$3552 = function(t$4203){return t$4203.length;
};
basis$0ByteTable$1$22.length_vector$3553 = function(v$3556){return v$3556.length;
};
basis$0ByteTable$1$22.explode$3557 = function(t$3560){var fix$4431 = {};
fix$4431.$h = function(v$3570,v$3571){lab$h: while (true) {if (v$3570 < 0) {return v$3571;
} else {var t$4432 = SmlPrims.chk_ovf_i32(v$3570 - 1);
var t$4433 = [t$3560[v$3570],v$3571];
var v$3570 = t$4432;
var v$3571 = t$4433;
continue lab$h;
};
};
};
var h$3561 = fix$4431.$h;
return h$3561(SmlPrims.chk_ovf_i32(t$3560.length - 1),null);
};
basis$0ByteTable$1$22.maxLen$3572 = 16777211;
basis$0ByteTable$1$22.sub$3573 = function(v$3586,v$3587){if ((v$3587 < 0)?true:(v$3587 >= v$3586.length)) {throw basis$0General$1.exn$Subscript$56;
} else {return v$3586[v$3587];
};
};
basis$0ByteTable$1$22.tabulate$3588 = function(v$3614,v$3615){if ((v$3614 < 0)?true:(v$3614 > 16777211)) {throw basis$0General$1.exn$Size$58;
} else {var t$3597 = Array(v$3614);
var fix$4434 = {};
fix$4434.$loop = function(j$3601){lab$loop: while (true) {if (j$3601 < v$3614) {(t$3597[j$3601] = (v$3615(j$3601)),0);
var t$4435 = SmlPrims.chk_ovf_i32(j$3601 + 1);
var j$3601 = t$4435;
continue lab$loop;
} else {return 0;
};
};
};
var loop$3598 = fix$4434.$loop;
loop$3598(0);
return t$3597;
};
};
basis$0ByteTable$1$22.array$3616 = function(v$3638,v$3639){if (v$3638 > 16777211) {throw basis$0General$1.exn$Size$58;
} else {var t$3625 = Array(v$3638);
var fix$4436 = {};
fix$4436.$loop = function(j$3629){lab$loop: while (true) {if (j$3629 < v$3638) {(t$3625[j$3629] = v$3639,0);
var t$4437 = SmlPrims.chk_ovf_i32(j$3629 + 1);
var j$3629 = t$4437;
continue lab$loop;
} else {return 0;
};
};
};
var loop$3626 = fix$4436.$loop;
loop$3626(0);
return t$3625;
};
};
basis$0ByteTable$1$22.update$3640 = function(v$3654,v$3655,v$3656){if ((v$3655 < 0)?true:(v$3655 >= v$3654.length)) {throw basis$0General$1.exn$Subscript$56;
} else {return (v$3654[v$3655] = v$3656,0);
};
};
basis$0ByteTable$1$22.updatev$3657 = function(v$3678,v$3679,v$3680){if ((v$3679 < 0)?true:(v$3679 >= v$3678.length)) {throw basis$0General$1.exn$Subscript$56;
} else {var v$4410 = v$3678.length;
if ((v$4410 < 0)?true:(v$4410 > 16777211)) {throw basis$0General$1.exn$Size$58;
} else {var t$4395 = Array(v$4410);
var fix$4438 = {};
fix$4438.$loop = function(j$4397){lab$loop: while (true) {if (j$4397 < v$4410) {(t$4395[j$4397] = ((v$3679 == j$4397)?v$3680:v$3678[j$4397]),0);
var t$4439 = SmlPrims.chk_ovf_i32(j$4397 + 1);
var j$4397 = t$4439;
continue lab$loop;
} else {return 0;
};
};
};
var loop$4396 = fix$4438.$loop;
loop$4396(0);
return t$4395;
};
};
};
basis$0ByteTable$1$22.foldl$3681 = function(f$3684,e$3687,a$3690){var stop$3691 = a$3690.length;
var fix$4440 = {};
fix$4440.$lr = function(v$3701,v$3702){lab$lr: while (true) {if (v$3701 < stop$3691) {var t$4441 = SmlPrims.chk_ovf_i32(v$3701 + 1);
var t$4442 = f$3684([a$3690[v$3701],v$3702]);
var v$3701 = t$4441;
var v$3702 = t$4442;
continue lab$lr;
} else {return v$3702;
};
};
};
var lr$3692 = fix$4440.$lr;
return lr$3692(0,e$3687);
};
basis$0ByteTable$1$22.foldr$3703 = function(f$3706,e$3709,a$3712){var fix$4443 = {};
fix$4443.$rl = function(v$3722,v$3723){lab$rl: while (true) {if (v$3722 >= 0) {var t$4444 = SmlPrims.chk_ovf_i32(v$3722 - 1);
var t$4445 = f$3706([a$3712[v$3722],v$3723]);
var v$3722 = t$4444;
var v$3723 = t$4445;
continue lab$rl;
} else {return v$3723;
};
};
};
var rl$3713 = fix$4443.$rl;
return rl$3713(SmlPrims.chk_ovf_i32(a$3712.length - 1),e$3709);
};
basis$0ByteTable$1$22.app$3724 = function(f$3727,a$3730){var stop$3731 = a$3730.length;
var fix$4446 = {};
fix$4446.$lr = function(j$3735){lab$lr: while (true) {if (j$3735 < stop$3731) {f$3727(a$3730[j$3735]);
var t$4447 = SmlPrims.chk_ovf_i32(j$3735 + 1);
var j$3735 = t$4447;
continue lab$lr;
} else {return 0;
};
};
};
var lr$3732 = fix$4446.$lr;
return lr$3732(0);
};
basis$0ByteTable$1$22.map$3742 = function(f$3745,a$3748){var v$4412 = a$3748.length;
if ((v$4412 < 0)?true:(v$4412 > 16777211)) {throw basis$0General$1.exn$Size$58;
} else {var t$4401 = Array(v$4412);
var fix$4448 = {};
fix$4448.$loop = function(j$4403){lab$loop: while (true) {if (j$4403 < v$4412) {(t$4401[j$4403] = (f$3745(a$3748[j$4403])),0);
var t$4449 = SmlPrims.chk_ovf_i32(j$4403 + 1);
var j$4403 = t$4449;
continue lab$loop;
} else {return 0;
};
};
};
var loop$4402 = fix$4448.$loop;
loop$4402(0);
return t$4401;
};
};
basis$0ByteTable$1$22.sliceend$3752 = function(v$4429,v$4430,v$3765){switch (v$3765[0]) { case 1: {if ((v$4430 < 0)?true:(v$4430 > v$4429.length)) {throw basis$0General$1.exn$Subscript$56;
} else {return v$4429.length;
};
 break; }default: {var v$3790 = v$3765[1];
if ((v$4430 < 0)?true:((v$3790 < 0)?true:((SmlPrims.chk_ovf_i32(v$4430 + v$3790)) > v$4429.length))) {throw basis$0General$1.exn$Subscript$56;
} else {return SmlPrims.chk_ovf_i32(v$4430 + v$3790);
};
} };
};
basis$0ByteTable$1$22.foldli$3791 = function(f$3794,e$3797,a$3800){var stop$4230 = a$3800.length;
var fix$4450 = {};
fix$4450.$lr = function(v$4233,v$4234){lab$lr: while (true) {if (v$4233 < stop$4230) {var t$4451 = SmlPrims.chk_ovf_i32(v$4233 + 1);
var t$4452 = f$3794([v$4233,a$3800[v$4233],v$4234]);
var v$4233 = t$4451;
var v$4234 = t$4452;
continue lab$lr;
} else {return v$4234;
};
};
};
var lr$4231 = fix$4450.$lr;
return lr$4231(0,e$3797);
};
basis$0ByteTable$1$22.foldri$3817 = function(f$3820,e$3823,a$3826){var start$4238 = SmlPrims.chk_ovf_i32(a$3826.length - 1);
var fix$4453 = {};
fix$4453.$rl = function(v$4241,v$4242){lab$rl: while (true) {if (v$4241 >= 0) {var t$4454 = SmlPrims.chk_ovf_i32(v$4241 - 1);
var t$4455 = f$3820([v$4241,a$3826[v$4241],v$4242]);
var v$4241 = t$4454;
var v$4242 = t$4455;
continue lab$rl;
} else {return v$4242;
};
};
};
var rl$4239 = fix$4453.$rl;
return rl$4239(start$4238,e$3823);
};
basis$0ByteTable$1$22.modifyi$3843 = function(f$3846,a$3849){var stop$3850 = a$3849.length;
var fix$4456 = {};
fix$4456.$lr = function(j$3854){lab$lr: while (true) {if (j$3854 < stop$3850) {(a$3849[j$3854] = (f$3846([j$3854,a$3849[j$3854]])),0);
var t$4457 = SmlPrims.chk_ovf_i32(j$3854 + 1);
var j$3854 = t$4457;
continue lab$lr;
} else {return 0;
};
};
};
var lr$3851 = fix$4456.$lr;
return lr$3851(0);
};
basis$0ByteTable$1$22.modify$3861 = function(f$3864,a$3867){var n$3868 = a$3867.length;
var fix$4458 = {};
fix$4458.$lr = function(j$3872){lab$lr: while (true) {if (j$3872 < n$3868) {(a$3867[j$3872] = (f$3864(a$3867[j$3872])),0);
var t$4459 = SmlPrims.chk_ovf_i32(j$3872 + 1);
var j$3872 = t$4459;
continue lab$lr;
} else {return 0;
};
};
};
var lr$3869 = fix$4458.$lr;
return lr$3869(0);
};
basis$0ByteTable$1$22.vector$3879 = function(a$3882){return SmlPrims.charArrayToString(a$3882);
};
basis$0ByteTable$1$22.copy$3949 = function(v$3962,v$3961,v$3960){var v$4349 = [1];
var n_dst$4264 = v$3961.length;
var n_src$4265 = v$3960.length;
var n$4266;
switch (v$4349[0]) { case 1: {n$4266 = (SmlPrims.chk_ovf_i32(v$3960.length - 0));
 break; }default: {n$4266 = v$4349[1];
} };
if ((n$4266 < 0)?true:(((SmlPrims.chk_ovf_i32(0 + n$4266)) > n_src$4265)?true:((v$3962 < 0)?true:((SmlPrims.chk_ovf_i32(v$3962 + n$4266)) > n_dst$4264)))) {throw basis$0General$1.exn$Subscript$56;
} else {if (0 < v$3962) {var fix$4460 = {};
fix$4460.$hdilo = function(j$4268){lab$hdilo: while (true) {if (j$4268 >= 0) {(v$3961[SmlPrims.chk_ovf_i32(v$3962 + j$4268)] = v$3960[SmlPrims.chk_ovf_i32(0 + j$4268)],0);
var t$4461 = SmlPrims.chk_ovf_i32(j$4268 - 1);
var j$4268 = t$4461;
continue lab$hdilo;
} else {return 0;
};
};
};
var hdilo$4267 = fix$4460.$hdilo;
return hdilo$4267(SmlPrims.chk_ovf_i32(n$4266 - 1));
} else {var fix$4462 = {};
fix$4462.$lo2hi = function(j$4272){lab$lo2hi: while (true) {if (j$4272 < n$4266) {(v$3961[SmlPrims.chk_ovf_i32(v$3962 + j$4272)] = v$3960[SmlPrims.chk_ovf_i32(0 + j$4272)],0);
var t$4463 = SmlPrims.chk_ovf_i32(j$4272 + 1);
var j$4272 = t$4463;
continue lab$lo2hi;
} else {return 0;
};
};
};
var lo2hi$4271 = fix$4462.$lo2hi;
return lo2hi$4271(0);
};
};
};
basis$0ByteTable$1$22.copyVec$4015 = function(v$4028,v$4027,v$4026){var v$4364 = [1];
var n_dst$4285 = v$4027.length;
var n_src$4286 = v$4026.length;
var n$4287;
switch (v$4364[0]) { case 1: {n$4287 = (SmlPrims.chk_ovf_i32(n_src$4286 - 0));
 break; }default: {n$4287 = v$4364[1];
} };
if ((n$4287 < 0)?true:(((SmlPrims.chk_ovf_i32(0 + n$4287)) > n_src$4286)?true:((v$4028 < 0)?true:((SmlPrims.chk_ovf_i32(v$4028 + n$4287)) > n_dst$4285)))) {throw basis$0General$1.exn$Subscript$56;
} else {var fix$4464 = {};
fix$4464.$lo2hi = function(j$4289){lab$lo2hi: while (true) {if (j$4289 < n$4287) {(v$4027[SmlPrims.chk_ovf_i32(v$4028 + j$4289)] = (v$4026.charCodeAt(SmlPrims.chk_ovf_i32(0 + j$4289))),0);
var t$4465 = SmlPrims.chk_ovf_i32(j$4289 + 1);
var j$4289 = t$4465;
continue lab$lo2hi;
} else {return 0;
};
};
};
var lo2hi$4288 = fix$4464.$lo2hi;
return lo2hi$4288(0);
};
};
basis$0ByteTable$1$22.appi$4029 = function(f$4032,a$4035){var stop$4036 = a$4035.length;
var fix$4466 = {};
fix$4466.$lr = function(j$4040){lab$lr: while (true) {if (j$4040 < stop$4036) {f$4032([j$4040,a$4035[j$4040]]);
var t$4467 = SmlPrims.chk_ovf_i32(j$4040 + 1);
var j$4040 = t$4467;
continue lab$lr;
} else {return 0;
};
};
};
var lr$4037 = fix$4466.$lr;
return lr$4037(0);
};
basis$0ByteTable$1$22.mapi$4047 = function(f$4050,a$4053){var v$4414 = a$4053.length;
if ((v$4414 < 0)?true:(v$4414 > 16777211)) {throw basis$0General$1.exn$Size$58;
} else {var t$4407 = Array(v$4414);
var fix$4468 = {};
fix$4468.$loop = function(j$4409){lab$loop: while (true) {if (j$4409 < v$4414) {(t$4407[j$4409] = (f$4050([j$4409,a$4053[j$4409]])),0);
var t$4469 = SmlPrims.chk_ovf_i32(j$4409 + 1);
var j$4409 = t$4469;
continue lab$loop;
} else {return 0;
};
};
};
var loop$4408 = fix$4468.$loop;
loop$4408(0);
return t$4407;
};
};
basis$0ByteTable$1$22.find$4057 = function(p$4060,a$4063){var stop$4064 = a$4063.length;
var fix$4470 = {};
fix$4470.$lr = function(j$4068){lab$lr: while (true) {if (j$4068 < stop$4064) {if (p$4060(a$4063[j$4068])) {return [0,a$4063[j$4068]];
} else {var t$4471 = SmlPrims.chk_ovf_i32(j$4068 + 1);
var j$4068 = t$4471;
continue lab$lr;
};
} else {return [1];
};
};
};
var lr$4065 = fix$4470.$lr;
return lr$4065(0);
};
basis$0ByteTable$1$22.exists$4077 = function(p$4080,a$4083){var stop$4084 = a$4083.length;
var fix$4472 = {};
fix$4472.$lr = function(j$4088){lab$lr: while (true) {if (j$4088 < stop$4084) {if (p$4080(a$4083[j$4088])) {return true;
} else {var t$4473 = SmlPrims.chk_ovf_i32(j$4088 + 1);
var j$4088 = t$4473;
continue lab$lr;
};
} else {return false;
};
};
};
var lr$4085 = fix$4472.$lr;
return lr$4085(0);
};
basis$0ByteTable$1$22.all$4097 = function(p$4100,a$4103){var stop$4104 = a$4103.length;
var fix$4474 = {};
fix$4474.$lr = function(j$4108){lab$lr: while (true) {if (j$4108 >= stop$4104) {return true;
} else {if (p$4100(a$4103[j$4108])) {var t$4475 = SmlPrims.chk_ovf_i32(j$4108 + 1);
var j$4108 = t$4475;
continue lab$lr;
} else {return false;
};
};
};
};
var lr$4105 = fix$4474.$lr;
return lr$4105(0);
};
basis$0ByteTable$1$22.findi$4117 = function(p$4120,a$4123){var stop$4124 = a$4123.length;
var fix$4476 = {};
fix$4476.$lr = function(j$4128){lab$lr: while (true) {if (j$4128 < stop$4124) {if (p$4120([j$4128,a$4123[j$4128]])) {return [0,[j$4128,a$4123[j$4128]]];
} else {var t$4477 = SmlPrims.chk_ovf_i32(j$4128 + 1);
var j$4128 = t$4477;
continue lab$lr;
};
} else {return [1];
};
};
};
var lr$4125 = fix$4476.$lr;
return lr$4125(0);
};
basis$0ByteTable$1$22.collate$4137 = function(cmp$4140,v$4144){var v$4173 = v$4144[0];
var v$4174 = v$4144[1];
var n1$4145 = v$4173.length;
var n2$4146 = v$4174.length;
var stop$4147 = (n1$4145 < n2$4146)?n1$4145:n2$4146;
var fix$4478 = {};
fix$4478.$h = function(j$4155){lab$h: while (true) {if (j$4155 == stop$4147) {return (n1$4145 < n2$4146)?0:((n1$4145 > n2$4146)?1:2);
} else {var v$4172 = cmp$4140([v$4173[j$4155],v$4174[j$4155]]);
switch (v$4172) { case 2: {var t$4479 = SmlPrims.chk_ovf_i32(j$4155 + 1);
var j$4155 = t$4479;
continue lab$h;
 break; }default: {return v$4172;
} };
};
};
};
var h$4152 = fix$4478.$h;
return h$4152(0);
};
return 0;
})();