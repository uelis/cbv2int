; ModuleID = '<stdin>'

@format = private unnamed_addr constant [3 x i8] c"%i\00"

declare i8* @salloc(i64)

declare i8* @spop(i64)

declare i8* @malloc(i64)

declare i8* @free(i8*)

define <{ i64, i64, i1 }> @Intmain(<{ i64, i1 }> %packed_arg) {
entry:
  %unpack = extractvalue <{ i64, i1 }> %packed_arg, 0
  %unpack1 = extractvalue <{ i64, i1 }> %packed_arg, 1
  br label %L12756

L12756:                                           ; preds = %entry
  %x = phi i1 [ %unpack1, %entry ]
  %x2 = phi i64 [ %unpack, %entry ]
  switch i1 %x, label %case0 [
    i1 true, label %case1
  ]

case1:                                            ; preds = %L12756
  br label %L12752

case0:                                            ; preds = %L12756
  br label %L12234

L12234:                                           ; preds = %case0
  ret <{ i64, i64, i1 }> <{ i64 undef, i64 undef, i1 false }>

L12752:                                           ; preds = %case1
  %memi8 = call i8* @salloc(i64 ptrtoint (<{ i64 }>* getelementptr (<{ i64 }>* null, i32 1) to i64))
  %memstruct = bitcast i8* %memi8 to <{ i64 }>*
  %pack = insertvalue <{ i64 }> undef, i64 %x2, 0
  store <{ i64 }> %pack, <{ i64 }>* %memstruct
  %memi83 = call i8* @salloc(i64 ptrtoint (<{ i1, i1 }>* getelementptr (<{ i1, i1 }>* null, i32 1) to i64))
  %memstruct4 = bitcast i8* %memi83 to <{ i1, i1 }>*
  store <{ i1, i1 }> <{ i1 false, i1 undef }>, <{ i1, i1 }>* %memstruct4
  %memi85 = call i8* @salloc(i64 ptrtoint (<{}>* getelementptr (<{}>* null, i32 1) to i64))
  %memstruct6 = bitcast i8* %memi85 to <{}>*
  store <{}> undef, <{}>* %memstruct6
  switch i1 true, label %case08 [
    i1 true, label %case17
  ]

case17:                                           ; preds = %L12752
  br label %L10740

case08:                                           ; preds = %L12752
  br label %L10742

L10742:                                           ; preds = %case0115, %case089, %case08
  %x9 = phi i1 [ true, %case08 ], [ %eq, %case089 ], [ %eq113, %case0115 ]
  %x10 = phi i64 [ 38, %case08 ], [ %unpack80, %case089 ], [ %add, %case0115 ]
  %x11 = phi i64 [ 38, %case08 ], [ %unpack80, %case089 ], [ %add, %case0115 ]
  %memi815 = call i8* @spop(i64 ptrtoint (<{}>* getelementptr (<{}>* null, i32 1) to i64))
  %memstruct16 = bitcast i8* %memi815 to <{}>*
  %lstruct = load <{}>* %memstruct16
  %memi817 = call i8* @spop(i64 ptrtoint (<{ i1, i1 }>* getelementptr (<{ i1, i1 }>* null, i32 1) to i64))
  %memstruct18 = bitcast i8* %memi817 to <{ i1, i1 }>*
  %lstruct19 = load <{ i1, i1 }>* %memstruct18
  %unpack20 = extractvalue <{ i1, i1 }> %lstruct19, 1
  %unpack21 = extractvalue <{ i1, i1 }> %lstruct19, 0
  switch i1 %unpack21, label %case023 [
    i1 true, label %case122
  ]

L10740:                                           ; preds = %case1114, %case188, %case17
  %x12 = phi i1 [ true, %case17 ], [ %eq, %case188 ], [ %eq113, %case1114 ]
  %x13 = phi i64 [ 38, %case17 ], [ %unpack80, %case188 ], [ %add, %case1114 ]
  %x14 = phi i64 [ 38, %case17 ], [ %unpack80, %case188 ], [ %add, %case1114 ]
  %add = add i64 %x13, -1
  %eq90 = icmp ne i64 %add, 0
  switch i1 %eq90, label %case092 [
    i1 true, label %case191
  ]

case122:                                          ; preds = %L10742
  br label %L78

case023:                                          ; preds = %L10742
  br label %L11661

L11661:                                           ; preds = %case0146, %case0102, %case023
  %x24 = phi i64 [ 1, %case023 ], [ 1, %case0102 ], [ %add136, %case0146 ]
  %memi845 = call i8* @spop(i64 ptrtoint (<{ i64 }>* getelementptr (<{ i64 }>* null, i32 1) to i64))
  %memstruct46 = bitcast i8* %memi845 to <{ i64 }>*
  %lstruct47 = load <{ i64 }>* %memstruct46
  %unpack48 = extractvalue <{ i64 }> %lstruct47, 0
  %i = call i64 @printf(i8* getelementptr inbounds ([3 x i8]* @format, i64 0, i64 0), i64 %x24)
  %pack49 = insertvalue <{ i64, i64, i1 }> undef, i64 %unpack48, 0
  %pack50 = insertvalue <{ i64, i64, i1 }> %pack49, i64 %x24, 1
  %pack51 = insertvalue <{ i64, i64, i1 }> %pack50, i1 true, 2
  ret <{ i64, i64, i1 }> %pack51

L78:                                              ; preds = %case1145, %case1101, %case122
  %x25 = phi i8* [ undef, %case122 ], [ undef, %case1101 ], [ undef, %case1145 ]
  %x26 = phi i8* [ undef, %case122 ], [ undef, %case1101 ], [ undef, %case1145 ]
  %x27 = phi i8* [ undef, %case122 ], [ undef, %case1101 ], [ undef, %case1145 ]
  %x28 = phi i1 [ false, %case122 ], [ false, %case1101 ], [ false, %case1145 ]
  %x29 = phi i1 [ true, %case122 ], [ true, %case1101 ], [ true, %case1145 ]
  %x30 = phi i1 [ true, %case122 ], [ true, %case1101 ], [ true, %case1145 ]
  %x31 = phi i1 [ true, %case122 ], [ true, %case1101 ], [ true, %case1145 ]
  %x32 = phi i1 [ undef, %case122 ], [ undef, %case1101 ], [ undef, %case1145 ]
  %x33 = phi i1 [ true, %case122 ], [ true, %case1101 ], [ true, %case1145 ]
  %x34 = phi i1 [ true, %case122 ], [ true, %case1101 ], [ true, %case1145 ]
  %x35 = phi i1 [ true, %case122 ], [ true, %case1101 ], [ true, %case1145 ]
  %x36 = phi i1 [ undef, %case122 ], [ undef, %case1101 ], [ undef, %case1145 ]
  %x37 = phi i1 [ true, %case122 ], [ true, %case1101 ], [ true, %case1145 ]
  %x38 = phi i1 [ true, %case122 ], [ true, %case1101 ], [ true, %case1145 ]
  %x39 = phi i1 [ %unpack21, %case122 ], [ %unpack100, %case1101 ], [ %unpack144, %case1145 ]
  %x40 = phi i1 [ %unpack20, %case122 ], [ %unpack99, %case1101 ], [ %unpack143, %case1145 ]
  %x41 = phi i1 [ %unpack20, %case122 ], [ %unpack99, %case1101 ], [ %unpack143, %case1145 ]
  %x42 = phi i64 [ 1, %case122 ], [ 1, %case1101 ], [ %add136, %case1145 ]
  %x43 = phi i64 [ 1, %case122 ], [ 1, %case1101 ], [ %add136, %case1145 ]
  %x44 = phi i64 [ 1, %case122 ], [ 1, %case1101 ], [ %add136, %case1145 ]
  switch i1 %x41, label %case053 [
    i1 true, label %case152
  ]

case152:                                          ; preds = %L78
  br label %L665

case053:                                          ; preds = %L78
  br label %L663

L663:                                             ; preds = %case053
  switch i1 %x37, label %case055 [
    i1 true, label %case154
  ]

L665:                                             ; preds = %case152
  switch i1 %x37, label %case0117 [
    i1 true, label %case1116
  ]

case154:                                          ; preds = %L663
  br label %L1147

case055:                                          ; preds = %L663
  br label %L1144

L1144:                                            ; preds = %case055
  %memi856 = call i8* @salloc(i64 ptrtoint (<{ i1, i1 }>* getelementptr (<{ i1, i1 }>* null, i32 1) to i64))
  %memstruct57 = bitcast i8* %memi856 to <{ i1, i1 }>*
  store <{ i1, i1 }> <{ i1 true, i1 false }>, <{ i1, i1 }>* %memstruct57
  %memptr = bitcast i8* %x27 to <{ i2, i1, i1, i1, i8* }>*
  %lstruct58 = load <{ i2, i1, i1, i1, i8* }>* %memptr
  %unpack59 = extractvalue <{ i2, i1, i1, i1, i8* }> %lstruct58, 0
  %unpack60 = extractvalue <{ i2, i1, i1, i1, i8* }> %lstruct58, 3
  %unpack61 = extractvalue <{ i2, i1, i1, i1, i8* }> %lstruct58, 2
  %unpack62 = extractvalue <{ i2, i1, i1, i1, i8* }> %lstruct58, 1
  %unpack63 = extractvalue <{ i2, i1, i1, i1, i8* }> %lstruct58, 4
  %free = call i8* @free(i8* %x27)
  switch i1 %unpack62, label %case065 [
    i1 true, label %case164
  ]

L1147:                                            ; preds = %case154
  switch i1 %x38, label %case073 [
    i1 true, label %case172
  ]

case164:                                          ; preds = %L1144
  br label %L697

case065:                                          ; preds = %L1144
  br label %L695

L695:                                             ; preds = %case0129, %case065
  unreachable

L697:                                             ; preds = %case1128, %case164
  %x66 = phi i8* [ %unpack63, %case164 ], [ %unpack126, %case1128 ]
  %x67 = phi i1 [ %unpack61, %case164 ], [ %unpack124, %case1128 ]
  %x68 = phi i1 [ %unpack60, %case164 ], [ %unpack123, %case1128 ]
  %x69 = phi i2 [ %unpack59, %case164 ], [ %unpack122, %case1128 ]
  switch i1 %x68, label %case071 [
    i1 true, label %case170
  ]

case170:                                          ; preds = %L697
  br label %L8075

case071:                                          ; preds = %L697
  br label %L7391

L7391:                                            ; preds = %case071
  unreachable

L8075:                                            ; preds = %case170
  unreachable

case172:                                          ; preds = %L1147
  br label %L1151

case073:                                          ; preds = %L1147
  br label %L1149

L1149:                                            ; preds = %case073
  unreachable

L1151:                                            ; preds = %case172
  %memi874 = call i8* @spop(i64 ptrtoint (<{}>* getelementptr (<{}>* null, i32 1) to i64))
  %memstruct75 = bitcast i8* %memi874 to <{}>*
  %lstruct76 = load <{}>* %memstruct75
  %memi877 = call i8* @spop(i64 ptrtoint (<{ i64 }>* getelementptr (<{ i64 }>* null, i32 1) to i64))
  %memstruct78 = bitcast i8* %memi877 to <{ i64 }>*
  %lstruct79 = load <{ i64 }>* %memstruct78
  %unpack80 = extractvalue <{ i64 }> %lstruct79, 0
  %memi881 = call i8* @salloc(i64 ptrtoint (<{ i64 }>* getelementptr (<{ i64 }>* null, i32 1) to i64))
  %memstruct82 = bitcast i8* %memi881 to <{ i64 }>*
  %pack83 = insertvalue <{ i64 }> undef, i64 %x44, 0
  store <{ i64 }> %pack83, <{ i64 }>* %memstruct82
  %memi884 = call i8* @salloc(i64 ptrtoint (<{ i1, i1 }>* getelementptr (<{ i1, i1 }>* null, i32 1) to i64))
  %memstruct85 = bitcast i8* %memi884 to <{ i1, i1 }>*
  store <{ i1, i1 }> <{ i1 true, i1 true }>, <{ i1, i1 }>* %memstruct85
  %memi886 = call i8* @salloc(i64 ptrtoint (<{}>* getelementptr (<{}>* null, i32 1) to i64))
  %memstruct87 = bitcast i8* %memi886 to <{}>*
  store <{}> undef, <{}>* %memstruct87
  %eq = icmp ne i64 %unpack80, 0
  switch i1 %eq, label %case089 [
    i1 true, label %case188
  ]

case188:                                          ; preds = %L1151
  br label %L10740

case089:                                          ; preds = %L1151
  br label %L10742

case191:                                          ; preds = %L10740
  br label %L7038

case092:                                          ; preds = %L10740
  br label %L7040

L7040:                                            ; preds = %case092
  %memi893 = call i8* @spop(i64 ptrtoint (<{}>* getelementptr (<{}>* null, i32 1) to i64))
  %memstruct94 = bitcast i8* %memi893 to <{}>*
  %lstruct95 = load <{}>* %memstruct94
  %memi896 = call i8* @spop(i64 ptrtoint (<{ i1, i1 }>* getelementptr (<{ i1, i1 }>* null, i32 1) to i64))
  %memstruct97 = bitcast i8* %memi896 to <{ i1, i1 }>*
  %lstruct98 = load <{ i1, i1 }>* %memstruct97
  %unpack99 = extractvalue <{ i1, i1 }> %lstruct98, 1
  %unpack100 = extractvalue <{ i1, i1 }> %lstruct98, 0
  switch i1 %unpack100, label %case0102 [
    i1 true, label %case1101
  ]

L7038:                                            ; preds = %case191
  %add103 = add i64 %add, -1
  %memi8104 = call i8* @salloc(i64 ptrtoint (<{ i64 }>* getelementptr (<{ i64 }>* null, i32 1) to i64))
  %memstruct105 = bitcast i8* %memi8104 to <{ i64 }>*
  %pack106 = insertvalue <{ i64 }> undef, i64 %add103, 0
  store <{ i64 }> %pack106, <{ i64 }>* %memstruct105
  %memi8107 = call i8* @salloc(i64 ptrtoint (<{}>* getelementptr (<{}>* null, i32 1) to i64))
  %memstruct108 = bitcast i8* %memi8107 to <{}>*
  store <{}> undef, <{}>* %memstruct108
  %memi8109 = call i8* @salloc(i64 ptrtoint (<{ i1, i1 }>* getelementptr (<{ i1, i1 }>* null, i32 1) to i64))
  %memstruct110 = bitcast i8* %memi8109 to <{ i1, i1 }>*
  store <{ i1, i1 }> <{ i1 true, i1 false }>, <{ i1, i1 }>* %memstruct110
  %memi8111 = call i8* @salloc(i64 ptrtoint (<{}>* getelementptr (<{}>* null, i32 1) to i64))
  %memstruct112 = bitcast i8* %memi8111 to <{}>*
  store <{}> undef, <{}>* %memstruct112
  %eq113 = icmp ne i64 %add, 0
  switch i1 %eq113, label %case0115 [
    i1 true, label %case1114
  ]

case1101:                                         ; preds = %L7040
  br label %L78

case0102:                                         ; preds = %L7040
  br label %L11661

case1114:                                         ; preds = %L7038
  br label %L10740

case0115:                                         ; preds = %L7038
  br label %L10742

case1116:                                         ; preds = %L665
  br label %L2347

case0117:                                         ; preds = %L665
  br label %L2344

L2344:                                            ; preds = %case0117
  %memi8118 = call i8* @salloc(i64 ptrtoint (<{ i1, i1 }>* getelementptr (<{ i1, i1 }>* null, i32 1) to i64))
  %memstruct119 = bitcast i8* %memi8118 to <{ i1, i1 }>*
  store <{ i1, i1 }> <{ i1 true, i1 true }>, <{ i1, i1 }>* %memstruct119
  %memptr120 = bitcast i8* %x27 to <{ i2, i1, i1, i1, i8* }>*
  %lstruct121 = load <{ i2, i1, i1, i1, i8* }>* %memptr120
  %unpack122 = extractvalue <{ i2, i1, i1, i1, i8* }> %lstruct121, 0
  %unpack123 = extractvalue <{ i2, i1, i1, i1, i8* }> %lstruct121, 3
  %unpack124 = extractvalue <{ i2, i1, i1, i1, i8* }> %lstruct121, 2
  %unpack125 = extractvalue <{ i2, i1, i1, i1, i8* }> %lstruct121, 1
  %unpack126 = extractvalue <{ i2, i1, i1, i1, i8* }> %lstruct121, 4
  %free127 = call i8* @free(i8* %x27)
  switch i1 %unpack125, label %case0129 [
    i1 true, label %case1128
  ]

L2347:                                            ; preds = %case1116
  switch i1 %x38, label %case0131 [
    i1 true, label %case1130
  ]

case1128:                                         ; preds = %L2344
  br label %L697

case0129:                                         ; preds = %L2344
  br label %L695

case1130:                                         ; preds = %L2347
  br label %L2351

case0131:                                         ; preds = %L2347
  br label %L2349

L2349:                                            ; preds = %case0131
  unreachable

L2351:                                            ; preds = %case1130
  %memi8132 = call i8* @spop(i64 ptrtoint (<{ i64 }>* getelementptr (<{ i64 }>* null, i32 1) to i64))
  %memstruct133 = bitcast i8* %memi8132 to <{ i64 }>*
  %lstruct134 = load <{ i64 }>* %memstruct133
  %unpack135 = extractvalue <{ i64 }> %lstruct134, 0
  %add136 = add i64 %unpack135, %x44
  %memi8137 = call i8* @spop(i64 ptrtoint (<{}>* getelementptr (<{}>* null, i32 1) to i64))
  %memstruct138 = bitcast i8* %memi8137 to <{}>*
  %lstruct139 = load <{}>* %memstruct138
  %memi8140 = call i8* @spop(i64 ptrtoint (<{ i1, i1 }>* getelementptr (<{ i1, i1 }>* null, i32 1) to i64))
  %memstruct141 = bitcast i8* %memi8140 to <{ i1, i1 }>*
  %lstruct142 = load <{ i1, i1 }>* %memstruct141
  %unpack143 = extractvalue <{ i1, i1 }> %lstruct142, 1
  %unpack144 = extractvalue <{ i1, i1 }> %lstruct142, 0
  switch i1 %unpack144, label %case0146 [
    i1 true, label %case1145
  ]

case1145:                                         ; preds = %L2351
  br label %L78

case0146:                                         ; preds = %L2351
  br label %L11661
}

declare i64 @printf(i8*, i64)

define i64 @main() {
start:
  %ret = call <{ i64, i64, i1 }> @Intmain(<{ i64, i1 }> undef)
  ret i64 0
}
