---
title: Z3S5 Lisp Reference Manual
titlepage: true
titlepage-background: ../Z3S5.png
footer-left: Version 2.4.5+7f8d143-gui.fyne2
author: by Erich Rast and all Help system contributors
date: 2025-4-17 18:37
header-includes: |
    \lstset{% for listings
        basicstyle=\footnotesize\ttfamily,
        breaklines=true,
    }
    \usepackage{xcolor}
---

For Z3S5 Lisp Version 2.4.5+7f8d143-gui.fyne2 with installed modules (gui-fileio gui fyne2 oop lib kvdb zimage tasks help beep unicode db fileio decimal ling float console base).

# Introduction

This is the reference manual for Z3S5 Lisp. This manual has been automatically generated from the entries of the online help system. The reference manual is divided into two large sections. Section [By Topics](#topics) lists functions and symbols organized by topics. Within each topic, entries are sorted alphabetically. Section [Complete Reference](#reference) lists all functions and symbols alphabetically.
Please consult the *User Manual* and the *Readme* document for more general information about Z3S5 Lisp, an introduction to its use, and how to embedd it into Go programs.

Incorrect documentation strings are bugs. Please report bugs using the corresponding [Github issue tracker for Z3S5 Lisp](https://github.com/rasteric/z3s5-lisp/issues) and be as precise as possible. Superfluous and missing documentation entries are misfeatures and may also be reported.

# Index {#idx}

[`%`](#link25) [`*`](#link2a) [`*colors*`](#link2a636f6c6f72732a) [`*error-handler*`](#link2a6572726f722d68616e646c65722a) [`*error-printer*`](#link2a6572726f722d7072696e7465722a) [`*help*`](#link2a68656c702a) [`*hooks*`](#link2a686f6f6b732a) [`*last-error*`](#link2a6c6173742d6572726f722a) [`*reflect*`](#link2a7265666c6563742a) [`+`](#link2b) [`-`](#link2d) [`/`](#link2f) [`/=`](#link2f3d) [`10th`](#link31307468) [`1st`](#link317374) [`2nd`](#link326e64) [`3rd`](#link337264) [`4th`](#link347468) [`5th`](#link357468) [`6th`](#link367468) [`7th`](#link377468) [`8th`](#link387468) [`9th`](#link397468) [`<`](#link3c) [`<=`](#link3c3d) [`=`](#link3d) [`>`](#link3e) [`>=`](#link3e3d) [`abs`](#link616273) [`action`](#link616374696f6e) [`action-start`](#link616374696f6e2d7374617274) [`action-stop`](#link616374696f6e2d73746f70) [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75) [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874) [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573) [`add-canvas-shortcut`](#link6164642d63616e7661732d73686f7274637574) [`add-hook`](#link6164642d686f6f6b) [`add-hook-internal`](#link6164642d686f6f6b2d696e7465726e616c) [`add-hook-once`](#link6164642d686f6f6b2d6f6e6365) [`add-lisp-editor`](#link6164642d6c6973702d656469746f72) [`add1`](#link61646431) [`alist->dict`](#link616c6973742d3e64696374) [`alist?`](#link616c6973743f) [`and`](#link616e64) [`append`](#link617070656e64) [`append-form`](#link617070656e642d666f726d) [`apply`](#link6170706c79) [`apropos`](#link6170726f706f73) [`array`](#link6172726179) [`array+`](#link61727261792b) [`array->list`](#link61727261792d3e6c697374) [`array->str`](#link61727261792d3e737472) [`array-append`](#link61727261792d617070656e64) [`array-copy`](#link61727261792d636f7079) [`array-exists?`](#link61727261792d6578697374733f) [`array-forall?`](#link61727261792d666f72616c6c3f) [`array-foreach`](#link61727261792d666f7265616368) [`array-len`](#link61727261792d6c656e) [`array-map!`](#link61727261792d6d617021) [`array-pmap!`](#link61727261792d706d617021) [`array-ref`](#link61727261792d726566) [`array-reverse`](#link61727261792d72657665727365) [`array-set`](#link61727261792d736574) [`array-slice`](#link61727261792d736c696365) [`array-sort`](#link61727261792d736f7274) [`array-walk`](#link61727261792d77616c6b) [`array?`](#link61727261793f) [`ascii85->blob`](#link617363696938352d3e626c6f62) [`assoc`](#link6173736f63) [`assoc1`](#link6173736f6331) [`assq`](#link61737371) [`atom?`](#link61746f6d3f) [`base64->blob`](#link6261736536342d3e626c6f62) [`beep`](#link62656570) [`bind`](#link62696e64) [`bitand`](#link626974616e64) [`bitclear`](#link626974636c656172) [`bitor`](#link6269746f72) [`bitshl`](#link62697473686c) [`bitshr`](#link626974736872) [`bitxor`](#link626974786f72) [`blob->ascii85`](#link626c6f622d3e61736369693835) [`blob->base64`](#link626c6f622d3e626173653634) [`blob->hex`](#link626c6f622d3e686578) [`blob->str`](#link626c6f622d3e737472) [`blob-chksum`](#link626c6f622d63686b73756d) [`blob-equal?`](#link626c6f622d657175616c3f) [`blob-free`](#link626c6f622d66726565) [`blob?`](#link626c6f623f) [`bool?`](#link626f6f6c3f) [`bound?`](#link626f756e643f) [`boxed?`](#link626f7865643f) [`build-array`](#link6275696c642d6172726179) [`build-list`](#link6275696c642d6c697374) [`caaar`](#link6361616172) [`caadr`](#link6361616472) [`caar`](#link63616172) [`cadar`](#link6361646172) [`caddr`](#link6361646472) [`cadr`](#link63616472) [`call-method`](#link63616c6c2d6d6574686f64) [`call-super`](#link63616c6c2d7375706572) [`can-externalize?`](#link63616e2d65787465726e616c697a653f) [`car`](#link636172) [`case`](#link63617365) [`ccmp`](#link63636d70) [`cdaar`](#link6364616172) [`cdadr`](#link6364616472) [`cdar`](#link63646172) [`cddar`](#link6364646172) [`cdddr`](#link6364646472) [`cddr`](#link63646472) [`cdec!`](#link6364656321) [`cdr`](#link636472) [`center-window-on-screen`](#link63656e7465722d77696e646f772d6f6e2d73637265656e) [`change-action-prefix`](#link6368616e67652d616374696f6e2d707265666978) [`change-all-action-prefixes`](#link6368616e67652d616c6c2d616374696f6e2d7072656669786573) [`char->str`](#link636861722d3e737472) [`chars`](#link6368617273) [`chars->str`](#link63686172732d3e737472) [`cinc!`](#link63696e6321) [`class-name`](#link636c6173732d6e616d65) [`class-of`](#link636c6173732d6f66) [`class?`](#link636c6173733f) [`close`](#link636c6f7365) [`close-gui`](#link636c6f73652d677569) [`close-window`](#link636c6f73652d77696e646f77) [`closure?`](#link636c6f737572653f) [`collect-garbage`](#link636f6c6c6563742d67617262616765) [`color`](#link636f6c6f72) [`color->color64`](#link636f6c6f722d3e636f6c6f723634) [`color64->color`](#link636f6c6f7236342d3e636f6c6f72) [`cons`](#link636f6e73) [`cons?`](#link636f6e733f) [`copy-record`](#link636f70792d7265636f7264) [`count-partitions`](#link636f756e742d706172746974696f6e73) [`count-text-grid-row-columns`](#link636f756e742d746578742d677269642d726f772d636f6c756d6e73) [`count-text-grid-rows`](#link636f756e742d746578742d677269642d726f7773) [`cpunum`](#link6370756e756d) [`create-lorem-ipsum`](#link6372656174652d6c6f72656d2d697073756d) [`cst!`](#link63737421) [`current-error-handler`](#link63757272656e742d6572726f722d68616e646c6572) [`current-zimage`](#link63757272656e742d7a696d616765) [`cwait`](#link6377616974) [`darken`](#link6461726b656e) [`date->epoch-ns`](#link646174652d3e65706f63682d6e73) [`datelist->epoch-ns`](#link646174656c6973742d3e65706f63682d6e73) [`datestr`](#link64617465737472) [`datestr*`](#link646174657374722a) [`datestr->datelist`](#link646174657374722d3e646174656c697374) [`day+`](#link6461792b) [`day-of-week`](#link6461792d6f662d7765656b) [`db.blob`](#link64622e626c6f62) [`db.close`](#link64622e636c6f7365) [`db.close-result`](#link64622e636c6f73652d726573756c74) [`db.exec`](#link64622e65786563) [`db.float`](#link64622e666c6f6174) [`db.int`](#link64622e696e74) [`db.open`](#link64622e6f70656e) [`db.open*`](#link64622e6f70656e2a) [`db.query`](#link64622e7175657279) [`db.result-column-count`](#link64622e726573756c742d636f6c756d6e2d636f756e74) [`db.result-columns`](#link64622e726573756c742d636f6c756d6e73) [`db.row`](#link64622e726f77) [`db.step`](#link64622e73746570) [`db.str`](#link64622e737472) [`deactivate-menu-child`](#link646561637469766174652d6d656e752d6368696c64) [`deactivate-menu-last-submenu`](#link646561637469766174652d6d656e752d6c6173742d7375626d656e75) [`declare-volatile`](#link6465636c6172652d766f6c6174696c65) [`def-custom-hook`](#link6465662d637573746f6d2d686f6f6b) [`default-error-handler`](#link64656661756c742d6572726f722d68616e646c6572) [`defclass`](#link646566636c617373) [`defmacro`](#link6465666d6163726f) [`defmethod`](#link6465666d6574686f64) [`defstruct`](#link646566737472756374) [`defun`](#link646566756e) [`delete`](#link64656c657465) [`delete-zedit-all`](#link64656c6574652d7a656469742d616c6c) [`dequeue!`](#link6465717565756521) [`dict`](#link64696374) [`dict->alist`](#link646963742d3e616c697374) [`dict->array`](#link646963742d3e6172726179) [`dict->keys`](#link646963742d3e6b657973) [`dict->list`](#link646963742d3e6c697374) [`dict->values`](#link646963742d3e76616c756573) [`dict-copy`](#link646963742d636f7079) [`dict-empty?`](#link646963742d656d7074793f) [`dict-foreach`](#link646963742d666f7265616368) [`dict-map`](#link646963742d6d6170) [`dict-map!`](#link646963742d6d617021) [`dict-merge`](#link646963742d6d65726765) [`dict-protect`](#link646963742d70726f74656374) [`dict-protected?`](#link646963742d70726f7465637465643f) [`dict-unprotect`](#link646963742d756e70726f74656374) [`dict?`](#link646963743f) [`dir`](#link646972) [`dir?`](#link6469723f) [`disable-object`](#link64697361626c652d6f626a656374) [`div`](#link646976) [`dolist`](#link646f6c697374) [`dotimes`](#link646f74696d6573) [`dump`](#link64756d70) [`dump-bindings`](#link64756d702d62696e64696e6773) [`enable-object`](#link656e61626c652d6f626a656374) [`enq`](#link656e71) [`enqueue!`](#link656e717565756521) [`entry-accepts-tab?`](#link656e7472792d616363657074732d7461623f) [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374) [`eq?`](#link65713f) [`eql?`](#link65716c3f) [`equal?`](#link657175616c3f) [`error`](#link6572726f72) [`error->str`](#link6572726f722d3e737472) [`error?`](#link6572726f723f) [`eval`](#link6576616c) [`even?`](#link6576656e3f) [`exists?`](#link6578697374733f) [`exit`](#link65786974) [`expand-macros`](#link657870616e642d6d6163726f73) [`expect`](#link657870656374) [`expect-err`](#link6578706563742d657272) [`expect-false`](#link6578706563742d66616c7365) [`expect-ok`](#link6578706563742d6f6b) [`expect-true`](#link6578706563742d74727565) [`expr->str`](#link657870722d3e737472) [`externalize`](#link65787465726e616c697a65) [`externalize0`](#link65787465726e616c697a6530) [`fdelete`](#link6664656c657465) [`feature?`](#link666561747572653f) [`file-display-name`](#link66696c652d646973706c61792d6e616d65) [`file-name`](#link66696c652d6e616d65) [`file-path`](#link66696c652d70617468) [`file-port?`](#link66696c652d706f72743f) [`file-suffix`](#link66696c652d737566666978) [`filter`](#link66696c746572) [`find-help-entries-with-incorrect-arity`](#link66696e642d68656c702d656e74726965732d776974682d696e636f72726563742d6172697479) [`find-missing-help-entries`](#link66696e642d6d697373696e672d68656c702d656e7472696573) [`find-unneeded-help-entries`](#link66696e642d756e6e65656465642d68656c702d656e7472696573) [`fl.abs`](#link666c2e616273) [`fl.acos`](#link666c2e61636f73) [`fl.asin`](#link666c2e6173696e) [`fl.asinh`](#link666c2e6173696e68) [`fl.atan`](#link666c2e6174616e) [`fl.atan2`](#link666c2e6174616e32) [`fl.atanh`](#link666c2e6174616e68) [`fl.cbrt`](#link666c2e63627274) [`fl.ceil`](#link666c2e6365696c) [`fl.cos`](#link666c2e636f73) [`fl.cosh`](#link666c2e636f7368) [`fl.dim`](#link666c2e64696d) [`fl.erf`](#link666c2e657266) [`fl.erfc`](#link666c2e65726663) [`fl.erfcinv`](#link666c2e65726663696e76) [`fl.erfinv`](#link666c2e657266696e76) [`fl.exp`](#link666c2e657870) [`fl.exp2`](#link666c2e65787032) [`fl.expm1`](#link666c2e6578706d31) [`fl.floor`](#link666c2e666c6f6f72) [`fl.fma`](#link666c2e666d61) [`fl.frexp`](#link666c2e6672657870) [`fl.gamma`](#link666c2e67616d6d61) [`fl.hypot`](#link666c2e6879706f74) [`fl.ilogb`](#link666c2e696c6f6762) [`fl.inf`](#link666c2e696e66) [`fl.is-nan?`](#link666c2e69732d6e616e3f) [`fl.j0`](#link666c2e6a30) [`fl.j1`](#link666c2e6a31) [`fl.jn`](#link666c2e6a6e) [`fl.ldexp`](#link666c2e6c64657870) [`fl.lgamma`](#link666c2e6c67616d6d61) [`fl.log`](#link666c2e6c6f67) [`fl.log10`](#link666c2e6c6f673130) [`fl.log1p`](#link666c2e6c6f673170) [`fl.log2`](#link666c2e6c6f6732) [`fl.logb`](#link666c2e6c6f6762) [`fl.max`](#link666c2e6d6178) [`fl.min`](#link666c2e6d696e) [`fl.mod`](#link666c2e6d6f64) [`fl.modf`](#link666c2e6d6f6466) [`fl.nan`](#link666c2e6e616e) [`fl.next-after`](#link666c2e6e6578742d6166746572) [`fl.pow`](#link666c2e706f77) [`fl.pow10`](#link666c2e706f773130) [`fl.remainder`](#link666c2e72656d61696e646572) [`fl.round`](#link666c2e726f756e64) [`fl.round-to-even`](#link666c2e726f756e642d746f2d6576656e) [`fl.signbit`](#link666c2e7369676e626974) [`fl.sin`](#link666c2e73696e) [`fl.sinh`](#link666c2e73696e68) [`fl.sqrt`](#link666c2e73717274) [`fl.tan`](#link666c2e74616e) [`fl.tanh`](#link666c2e74616e68) [`fl.trunc`](#link666c2e7472756e63) [`fl.y0`](#link666c2e7930) [`fl.y1`](#link666c2e7931) [`fl.yn`](#link666c2e796e) [`flatten`](#link666c617474656e) [`float`](#link666c6f6174) [`fmt`](#link666d74) [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374) [`focus-next-canvas-object`](#link666f6375732d6e6578742d63616e7661732d6f626a656374) [`focus-previous-canvas-object`](#link666f6375732d70726576696f75732d63616e7661732d6f626a656374) [`focus-zedit`](#link666f6375732d7a65646974) [`forall?`](#link666f72616c6c3f) [`force`](#link666f726365) [`foreach`](#link666f7265616368) [`forget`](#link666f72676574) [`forget-gui-object`](#link666f726765742d6775692d6f626a656374) [`functional-arity`](#link66756e6374696f6e616c2d6172697479) [`functional-arity*`](#link66756e6374696f6e616c2d61726974792a) [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f) [`functional?`](#link66756e6374696f6e616c3f) [`gensym`](#link67656e73796d) [`get`](#link676574) [`get-action`](#link6765742d616374696f6e) [`get-clipboard-content`](#link6765742d636c6970626f6172642d636f6e74656e74) [`get-device-info`](#link6765742d6465766963652d696e666f) [`get-entry-cursor`](#link6765742d656e7472792d637572736f72) [`get-entry-cursor-pos`](#link6765742d656e7472792d637572736f722d706f73) [`get-focused-canvas-object`](#link6765742d666f63757365642d63616e7661732d6f626a656374) [`get-label-text`](#link6765742d6c6162656c2d74657874) [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c) [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65) [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e) [`get-object-size`](#link6765742d6f626a6563742d73697a65) [`get-or-set`](#link6765742d6f722d736574) [`get-partitions`](#link6765742d706172746974696f6e73) [`get-progress-bar-value`](#link6765742d70726f67726573732d6261722d76616c7565) [`get-scroll-offset`](#link6765742d7363726f6c6c2d6f6666736574) [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c) [`get-text-grid-cell-size`](#link6765742d746578742d677269642d63656c6c2d73697a65) [`get-text-grid-row`](#link6765742d746578742d677269642d726f77) [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874) [`get-text-grid-rune`](#link6765742d746578742d677269642d72756e65) [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468) [`get-text-grid-text`](#link6765742d746578742d677269642d74657874) [`get-window-canvas`](#link6765742d77696e646f772d63616e766173) [`get-window-content`](#link6765742d77696e646f772d636f6e74656e74) [`get-window-icon`](#link6765742d77696e646f772d69636f6e) [`get-window-main-menu`](#link6765742d77696e646f772d6d61696e2d6d656e75) [`get-window-title`](#link6765742d77696e646f772d7469746c65) [`get-zedit-char-at`](#link6765742d7a656469742d636861722d6174) [`get-zedit-columns`](#link6765742d7a656469742d636f6c756d6e73) [`get-zedit-config`](#link6765742d7a656469742d636f6e666967) [`get-zedit-last-column`](#link6765742d7a656469742d6c6173742d636f6c756d6e) [`get-zedit-last-line`](#link6765742d7a656469742d6c6173742d6c696e65) [`get-zedit-lines`](#link6765742d7a656469742d6c696e6573) [`get-zedit-next-pos`](#link6765742d7a656469742d6e6578742d706f73) [`get-zedit-prev-pos`](#link6765742d7a656469742d707265762d706f73) [`get-zedit-text`](#link6765742d7a656469742d74657874) [`get-zedit-text-range`](#link6765742d7a656469742d746578742d72616e6765) [`get-zedit-top-line`](#link6765742d7a656469742d746f702d6c696e65) [`getstacked`](#link676574737461636b6564) [`glance`](#link676c616e6365) [`global-startup-time`](#link676c6f62616c2d737461727475702d74696d65) [`global-sym?`](#link676c6f62616c2d73796d3f) [`gui`](#link677569) [`gui*`](#link6775692a) [`gui+`](#link6775692b) [`has`](#link686173) [`has-action-system?`](#link6861732d616374696f6e2d73797374656d3f) [`has-action?`](#link6861732d616374696f6e3f) [`has-key?`](#link6861732d6b65793f) [`has-method?`](#link6861732d6d6574686f643f) [`has-prop?`](#link6861732d70726f703f) [`help`](#link68656c70) [`help->manual-entry`](#link68656c702d3e6d616e75616c2d656e747279) [`help-about`](#link68656c702d61626f7574) [`help-entry`](#link68656c702d656e747279) [`help-strings`](#link68656c702d737472696e6773) [`help-topic-info`](#link68656c702d746f7069632d696e666f) [`help-topics`](#link68656c702d746f70696373) [`hex->blob`](#link6865782d3e626c6f62) [`hide-object`](#link686964652d6f626a656374) [`hide-window`](#link686964652d77696e646f77) [`hook`](#link686f6f6b) [`hour+`](#link686f75722b) [`identity`](#link6964656e74697479) [`if`](#link6966) [`inchars`](#link696e6368617273) [`include`](#link696e636c756465) [`index`](#link696e646578) [`init-actions`](#link696e69742d616374696f6e73) [`init-remember`](#link696e69742d72656d656d626572) [`insert-text-grid-row`](#link696e736572742d746578742d677269642d726f77) [`instr`](#link696e737472) [`int`](#link696e74) [`intern`](#link696e7465726e) [`internalize`](#link696e7465726e616c697a65) [`intrinsic`](#link696e7472696e736963) [`intrinsic?`](#link696e7472696e7369633f) [`isa?`](#link6973613f) [`iterate`](#link69746572617465) [`kvdb.begin`](#link6b7664622e626567696e) [`kvdb.close`](#link6b7664622e636c6f7365) [`kvdb.commit`](#link6b7664622e636f6d6d6974) [`kvdb.db?`](#link6b7664622e64623f) [`kvdb.forget`](#link6b7664622e666f72676574) [`kvdb.forget-everything`](#link6b7664622e666f726765742d65766572797468696e67) [`kvdb.get`](#link6b7664622e676574) [`kvdb.info`](#link6b7664622e696e666f) [`kvdb.open`](#link6b7664622e6f70656e) [`kvdb.rollback`](#link6b7664622e726f6c6c6261636b) [`kvdb.search`](#link6b7664622e736561726368) [`kvdb.set`](#link6b7664622e736574) [`kvdb.when`](#link6b7664622e7768656e) [`last`](#link6c617374) [`lcons`](#link6c636f6e73) [`len`](#link6c656e) [`let`](#link6c6574) [`letrec`](#link6c6574726563) [`lighten`](#link6c69676874656e) [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e) [`ling.hamming`](#link6c696e672e68616d6d696e67) [`ling.jaro`](#link6c696e672e6a61726f) [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572) [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e) [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578) [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265) [`ling.metaphone`](#link6c696e672e6d65746170686f6e65) [`ling.nysiis`](#link6c696e672e6e7973696973) [`ling.porter`](#link6c696e672e706f72746572) [`ling.soundex`](#link6c696e672e736f756e646578) [`list`](#link6c697374) [`list->array`](#link6c6973742d3e6172726179) [`list->set`](#link6c6973742d3e736574) [`list->str`](#link6c6973742d3e737472) [`list-exists?`](#link6c6973742d6578697374733f) [`list-forall?`](#link6c6973742d666f72616c6c3f) [`list-foreach`](#link6c6973742d666f7265616368) [`list-last`](#link6c6973742d6c617374) [`list-ref`](#link6c6973742d726566) [`list-reverse`](#link6c6973742d72657665727365) [`list-slice`](#link6c6973742d736c696365) [`list-uri`](#link6c6973742d757269) [`list?`](#link6c6973743f) [`listable-uri?`](#link6c69737461626c652d7572693f) [`load`](#link6c6f6164) [`load-zimage`](#link6c6f61642d7a696d616765) [`macro?`](#link6d6163726f3f) [`make`](#link6d616b65) [`make*`](#link6d616b652a) [`make-blob`](#link6d616b652d626c6f62) [`make-mutex`](#link6d616b652d6d75746578) [`make-or-get-zedit-style-tag`](#link6d616b652d6f722d6765742d7a656469742d7374796c652d746167) [`make-queue`](#link6d616b652d7175657565) [`make-set`](#link6d616b652d736574) [`make-stack`](#link6d616b652d737461636b) [`make-symbol`](#link6d616b652d73796d626f6c) [`map`](#link6d6170) [`map-pairwise`](#link6d61702d7061697277697365) [`mapcar`](#link6d6170636172) [`max`](#link6d6178) [`member`](#link6d656d626572) [`memq`](#link6d656d71) [`memstats`](#link6d656d7374617473) [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f) [`menu-item-disabled?`](#link6d656e752d6974656d2d64697361626c65643f) [`methods`](#link6d6574686f6473) [`min`](#link6d696e) [`minmax`](#link6d696e6d6178) [`minute+`](#link6d696e7574652b) [`mod`](#link6d6f64) [`month+`](#link6d6f6e74682b) [`move-object`](#link6d6f76652d6f626a656374) [`mutex-lock`](#link6d757465782d6c6f636b) [`mutex-rlock`](#link6d757465782d726c6f636b) [`mutex-runlock`](#link6d757465782d72756e6c6f636b) [`mutex-unlock`](#link6d757465782d756e6c6f636b) [`nconc`](#link6e636f6e63) [`new`](#link6e6577) [`new-app-tabs`](#link6e65772d6170702d74616273) [`new-border`](#link6e65772d626f72646572) [`new-button`](#link6e65772d627574746f6e) [`new-button-with-icon`](#link6e65772d627574746f6e2d776974682d69636f6e) [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574) [`new-check`](#link6e65772d636865636b) [`new-choice`](#link6e65772d63686f696365) [`new-circle`](#link6e65772d636972636c65) [`new-combined-string-validator`](#link6e65772d636f6d62696e65642d737472696e672d76616c696461746f72) [`new-container`](#link6e65772d636f6e7461696e6572) [`new-container-without-layout`](#link6e65772d636f6e7461696e65722d776974686f75742d6c61796f7574) [`new-doc-tabs`](#link6e65772d646f632d74616273) [`new-entry`](#link6e65772d656e747279) [`new-form`](#link6e65772d666f726d) [`new-form-item`](#link6e65772d666f726d2d6974656d) [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574) [`new-grid-layout`](#link6e65772d677269642d6c61796f7574) [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574) [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574) [`new-hscroll`](#link6e65772d687363726f6c6c) [`new-hsplit`](#link6e65772d6873706c6974) [`new-hyperlink`](#link6e65772d68797065726c696e6b) [`new-icon`](#link6e65772d69636f6e) [`new-image-from-file`](#link6e65772d696d6167652d66726f6d2d66696c65) [`new-image-from-resource`](#link6e65772d696d6167652d66726f6d2d7265736f75726365) [`new-label`](#link6e65772d6c6162656c) [`new-line`](#link6e65772d6c696e65) [`new-list`](#link6e65772d6c697374) [`new-main-menu`](#link6e65772d6d61696e2d6d656e75) [`new-menu`](#link6e65772d6d656e75) [`new-menu*`](#link6e65772d6d656e752a) [`new-menu-item`](#link6e65772d6d656e752d6974656d) [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72) [`new-progress-bar`](#link6e65772d70726f67726573732d626172) [`new-raster-with-pixels`](#link6e65772d7261737465722d776974682d706978656c73) [`new-rectangle`](#link6e65772d72656374616e676c65) [`new-regexp-validator`](#link6e65772d7265676578702d76616c696461746f72) [`new-scroll`](#link6e65772d7363726f6c6c) [`new-slider`](#link6e65772d736c69646572) [`new-spacer`](#link6e65772d737061636572) [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574) [`new-struct`](#link6e65772d737472756374) [`new-tabitem`](#link6e65772d7461626974656d) [`new-tabitem-with-icon`](#link6e65772d7461626974656d2d776974682d69636f6e) [`new-table`](#link6e65772d7461626c65) [`new-text`](#link6e65772d74657874) [`new-text-grid`](#link6e65772d746578742d67726964) [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72) [`new-tree`](#link6e65772d74726565) [`new-validator`](#link6e65772d76616c696461746f72) [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574) [`new-vscroll`](#link6e65772d767363726f6c6c) [`new-vsplit`](#link6e65772d7673706c6974) [`new-window`](#link6e65772d77696e646f77) [`new-zedit`](#link6e65772d7a65646974) [`nl`](#link6e6c) [`nonce`](#link6e6f6e6365) [`not`](#link6e6f74) [`now`](#link6e6f77) [`now-ms`](#link6e6f772d6d73) [`now-ns`](#link6e6f772d6e73) [`nreverse`](#link6e72657665727365) [`nrgba`](#link6e72676261) [`nrgba64`](#link6e726762613634) [`nth`](#link6e7468) [`nth-partition`](#link6e74682d706172746974696f6e) [`nthdef`](#link6e7468646566) [`null?`](#link6e756c6c3f) [`num?`](#link6e756d3f) [`object-disabled?`](#link6f626a6563742d64697361626c65643f) [`object?`](#link6f626a6563743f) [`odd?`](#link6f64643f) [`on-feature`](#link6f6e2d66656174757265) [`open`](#link6f70656e) [`or`](#link6f72) [`out`](#link6f7574) [`outy`](#link6f757479) [`peek`](#link7065656b) [`permission?`](#link7065726d697373696f6e3f) [`permissions`](#link7065726d697373696f6e73) [`poke`](#link706f6b65) [`pop!`](#link706f7021) [`pop-error-handler`](#link706f702d6572726f722d68616e646c6572) [`pop-finalizer`](#link706f702d66696e616c697a6572) [`popstacked`](#link706f70737461636b6564) [`prin1`](#link7072696e31) [`princ`](#link7072696e63) [`print`](#link7072696e74) [`print-zedit`](#link7072696e742d7a65646974) [`proc?`](#link70726f633f) [`prop`](#link70726f70) [`props`](#link70726f7073) [`protect`](#link70726f74656374) [`protect-toplevel-symbols`](#link70726f746563742d746f706c6576656c2d73796d626f6c73) [`protected?`](#link70726f7465637465643f) [`prune-task-table`](#link7072756e652d7461736b2d7461626c65) [`prune-unneeded-help-entries`](#link7072756e652d756e6e65656465642d68656c702d656e7472696573) [`push!`](#link7075736821) [`push-error-handler`](#link707573682d6572726f722d68616e646c6572) [`push-finalizer`](#link707573682d66696e616c697a6572) [`pushstacked`](#link70757368737461636b6564) [`queue-empty?`](#link71756575652d656d7074793f) [`queue-len`](#link71756575652d6c656e) [`queue?`](#link71756575653f) [`rand`](#link72616e64) [`random-color`](#link72616e646f6d2d636f6c6f72) [`read`](#link72656164) [`read-binary`](#link726561642d62696e617279) [`read-string`](#link726561642d737472696e67) [`read-zimage`](#link726561642d7a696d616765) [`readall`](#link72656164616c6c) [`readall-str`](#link72656164616c6c2d737472) [`recall`](#link726563616c6c) [`recall-info`](#link726563616c6c2d696e666f) [`recall-when`](#link726563616c6c2d7768656e) [`recollect`](#link7265636f6c6c656374) [`record?`](#link7265636f72643f) [`refresh-main-menu`](#link726566726573682d6d61696e2d6d656e75) [`refresh-menu*`](#link726566726573682d6d656e752a) [`refresh-object`](#link726566726573682d6f626a656374) [`register-action`](#link72656769737465722d616374696f6e) [`remember`](#link72656d656d626572) [`remove-canvas-shortcut`](#link72656d6f76652d63616e7661732d73686f7274637574) [`remove-duplicates`](#link72656d6f76652d6475706c696361746573) [`remove-hook`](#link72656d6f76652d686f6f6b) [`remove-hook-internal`](#link72656d6f76652d686f6f6b2d696e7465726e616c) [`remove-hooks`](#link72656d6f76652d686f6f6b73) [`remove-text-grid-row`](#link72656d6f76652d746578742d677269642d726f77) [`remove-zedit-event-handler`](#link72656d6f76652d7a656469742d6576656e742d68616e646c6572) [`rename-action`](#link72656e616d652d616374696f6e) [`replace-hook`](#link7265706c6163652d686f6f6b) [`reset-color`](#link72657365742d636f6c6f72) [`resize-object`](#link726573697a652d6f626a656374) [`reverse`](#link72657665727365) [`rnd`](#link726e64) [`rndseed`](#link726e6473656564) [`rplaca`](#link72706c616361) [`run-at`](#link72756e2d6174) [`run-hook`](#link72756e2d686f6f6b) [`run-hook-internal`](#link72756e2d686f6f6b2d696e7465726e616c) [`run-selftest`](#link72756e2d73656c6674657374) [`run-zimage`](#link72756e2d7a696d616765) [`sanity-check-help-system`](#link73616e6974792d636865636b2d68656c702d73797374656d) [`save-zimage`](#link736176652d7a696d616765) [`sec+`](#link7365632b) [`semver.build`](#link73656d7665722e6275696c64) [`semver.canonical`](#link73656d7665722e63616e6f6e6963616c) [`semver.compare`](#link73656d7665722e636f6d70617265) [`semver.is-valid?`](#link73656d7665722e69732d76616c69643f) [`semver.major`](#link73656d7665722e6d616a6f72) [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72) [`semver.max`](#link73656d7665722e6d6178) [`semver.prerelease`](#link73656d7665722e70726572656c65617365) [`seq?`](#link7365713f) [`set`](#link736574) [`set*`](#link7365742a) [`set->list`](#link7365742d3e6c697374) [`set-app-metadata`](#link7365742d6170702d6d65746164617461) [`set-canvas-on-typed-key`](#link7365742d63616e7661732d6f6e2d74797065642d6b6579) [`set-canvas-on-typed-rune`](#link7365742d63616e7661732d6f6e2d74797065642d72756e65) [`set-clipboard-content`](#link7365742d636c6970626f6172642d636f6e74656e74) [`set-color`](#link7365742d636f6c6f72) [`set-complement`](#link7365742d636f6d706c656d656e74) [`set-difference`](#link7365742d646966666572656e6365) [`set-element?`](#link7365742d656c656d656e743f) [`set-empty?`](#link7365742d656d7074793f) [`set-entry-cursor-column`](#link7365742d656e7472792d637572736f722d636f6c756d6e) [`set-entry-cursor-row`](#link7365742d656e7472792d637572736f722d726f77) [`set-entry-min-rows-visible`](#link7365742d656e7472792d6d696e2d726f77732d76697369626c65) [`set-entry-on-change-callback`](#link7365742d656e7472792d6f6e2d6368616e67652d63616c6c6261636b) [`set-entry-on-cursor-change-callback`](#link7365742d656e7472792d6f6e2d637572736f722d6368616e67652d63616c6c6261636b) [`set-entry-place-holder`](#link7365742d656e7472792d706c6163652d686f6c646572) [`set-entry-text`](#link7365742d656e7472792d74657874) [`set-entry-text-wrap`](#link7365742d656e7472792d746578742d77726170) [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72) [`set-equal?`](#link7365742d657175616c3f) [`set-help-topic-info`](#link7365742d68656c702d746f7069632d696e666f) [`set-intersection`](#link7365742d696e74657273656374696f6e) [`set-label-text`](#link7365742d6c6162656c2d74657874) [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564) [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564) [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c) [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b) [`set-permissions`](#link7365742d7065726d697373696f6e73) [`set-progress-bar`](#link7365742d70726f67726573732d626172) [`set-scroll-offset`](#link7365742d7363726f6c6c2d6f6666736574) [`set-select-options`](#link7365742d73656c6563742d6f7074696f6e73) [`set-slider-value`](#link7365742d736c696465722d76616c7565) [`set-split-offset`](#link7365742d73706c69742d6f6666736574) [`set-subset?`](#link7365742d7375627365743f) [`set-text-alignment`](#link7365742d746578742d616c69676e6d656e74) [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c) [`set-text-grid-row`](#link7365742d746578742d677269642d726f77) [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65) [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65) [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273) [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365) [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65) [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765) [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468) [`set-text-grid-text`](#link7365742d746578742d677269642d74657874) [`set-text-size`](#link7365742d746578742d73697a65) [`set-text-style`](#link7365742d746578742d7374796c65) [`set-union`](#link7365742d756e696f6e) [`set-volume`](#link7365742d766f6c756d65) [`set-window-content`](#link7365742d77696e646f772d636f6e74656e74) [`set-window-full-screen`](#link7365742d77696e646f772d66756c6c2d73637265656e) [`set-window-icon`](#link7365742d77696e646f772d69636f6e) [`set-window-main-menu`](#link7365742d77696e646f772d6d61696e2d6d656e75) [`set-window-on-close-callback`](#link7365742d77696e646f772d6f6e2d636c6f73652d63616c6c6261636b) [`set-window-padded`](#link7365742d77696e646f772d706164646564) [`set-window-size`](#link7365742d77696e646f772d73697a65) [`set-window-title`](#link7365742d77696e646f772d7469746c65) [`set-zedit-config`](#link7365742d7a656469742d636f6e666967) [`set-zedit-event-handler`](#link7365742d7a656469742d6576656e742d68616e646c6572) [`set-zedit-line-number-style`](#link7365742d7a656469742d6c696e652d6e756d6265722d7374796c65) [`set-zedit-text`](#link7365742d7a656469742d74657874) [`set-zedit-top-line`](#link7365742d7a656469742d746f702d6c696e65) [`set?`](#link7365743f) [`setcar`](#link736574636172) [`setcdr`](#link736574636472) [`setprop`](#link73657470726f70) [`shorten`](#link73686f7274656e) [`show-color-picker`](#link73686f772d636f6c6f722d7069636b6572) [`show-confirm`](#link73686f772d636f6e6669726d) [`show-custom`](#link73686f772d637573746f6d) [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d) [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73) [`show-file-open`](#link73686f772d66696c652d6f70656e) [`show-file-save`](#link73686f772d66696c652d73617665) [`show-folder-open`](#link73686f772d666f6c6465722d6f70656e) [`show-form`](#link73686f772d666f726d) [`show-information`](#link73686f772d696e666f726d6174696f6e) [`show-object`](#link73686f772d6f626a656374) [`sleep`](#link736c656570) [`sleep-ns`](#link736c6565702d6e73) [`slice`](#link736c696365) [`sort`](#link736f7274) [`sort-symbols`](#link736f72742d73796d626f6c73) [`spaces`](#link737061636573) [`stack-empty?`](#link737461636b2d656d7074793f) [`stack-len`](#link737461636b2d6c656e) [`stack?`](#link737461636b3f) [`str+`](#link7374722b) [`str->array`](#link7374722d3e6172726179) [`str->blob`](#link7374722d3e626c6f62) [`str->char`](#link7374722d3e63686172) [`str->chars`](#link7374722d3e6368617273) [`str->expr`](#link7374722d3e65787072) [`str->expr*`](#link7374722d3e657870722a) [`str->list`](#link7374722d3e6c697374) [`str->sym`](#link7374722d3e73796d) [`str-count-substr`](#link7374722d636f756e742d737562737472) [`str-empty?`](#link7374722d656d7074793f) [`str-exists?`](#link7374722d6578697374733f) [`str-forall?`](#link7374722d666f72616c6c3f) [`str-foreach`](#link7374722d666f7265616368) [`str-index`](#link7374722d696e646578) [`str-join`](#link7374722d6a6f696e) [`str-port?`](#link7374722d706f72743f) [`str-ref`](#link7374722d726566) [`str-remove-number`](#link7374722d72656d6f76652d6e756d626572) [`str-remove-prefix`](#link7374722d72656d6f76652d707265666978) [`str-remove-suffix`](#link7374722d72656d6f76652d737566666978) [`str-replace`](#link7374722d7265706c616365) [`str-replace*`](#link7374722d7265706c6163652a) [`str-reverse`](#link7374722d72657665727365) [`str-segment`](#link7374722d7365676d656e74) [`str-slice`](#link7374722d736c696365) [`str?`](#link7374723f) [`strbuild`](#link7374726275696c64) [`strcase`](#link73747263617365) [`strcenter`](#link73747263656e746572) [`strcnt`](#link737472636e74) [`strleft`](#link7374726c656674) [`strlen`](#link7374726c656e) [`strless`](#link7374726c657373) [`strlimit`](#link7374726c696d6974) [`strmap`](#link7374726d6170) [`stropen`](#link7374726f70656e) [`strright`](#link7374727269676874) [`strsplit`](#link73747273706c6974) [`struct-index`](#link7374727563742d696e646578) [`struct-instantiate`](#link7374727563742d696e7374616e7469617465) [`struct-name`](#link7374727563742d6e616d65) [`struct-props`](#link7374727563742d70726f7073) [`struct-size`](#link7374727563742d73697a65) [`struct?`](#link7374727563743f) [`sub1`](#link73756231) [`supers`](#link737570657273) [`sym->str`](#link73796d2d3e737472) [`sym?`](#link73796d3f) [`synout`](#link73796e6f7574) [`synouty`](#link73796e6f757479) [`sys-key?`](#link7379732d6b65793f) [`sysmsg`](#link7379736d7367) [`sysmsg*`](#link7379736d73672a) [`take`](#link74616b65) [`task`](#link7461736b) [`task-broadcast`](#link7461736b2d62726f616463617374) [`task-recv`](#link7461736b2d72656376) [`task-remove`](#link7461736b2d72656d6f7665) [`task-run`](#link7461736b2d72756e) [`task-schedule`](#link7461736b2d7363686564756c65) [`task-send`](#link7461736b2d73656e64) [`task-state`](#link7461736b2d7374617465) [`task?`](#link7461736b3f) [`terpri`](#link746572707269) [`testing`](#link74657374696e67) [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f) [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f) [`the-color`](#link7468652d636f6c6f72) [`the-color-names`](#link7468652d636f6c6f722d6e616d6573) [`theme-color`](#link7468656d652d636f6c6f72) [`theme-icon`](#link7468656d652d69636f6e) [`theme-is-dark?`](#link7468656d652d69732d6461726b3f) [`time`](#link74696d65) [`trigger-menu-last`](#link747269676765722d6d656e752d6c617374) [`truncate`](#link7472756e63617465) [`try`](#link747279) [`type-of`](#link747970652d6f66) [`type-of*`](#link747970652d6f662a) [`unfocus-canvas-objects`](#link756e666f6375732d63616e7661732d6f626a65637473) [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f) [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f) [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f) [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f) [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f) [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f) [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f) [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f) [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f) [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f) [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f) [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f) [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f) [`unless`](#link756e6c657373) [`unprotect`](#link756e70726f74656374) [`unprotect-toplevel-symbols`](#link756e70726f746563742d746f706c6576656c2d73796d626f6c73) [`until`](#link756e74696c) [`uri?`](#link7572693f) [`valid?`](#link76616c69643f) [`validate-object`](#link76616c69646174652d6f626a656374) [`void`](#link766f6964) [`void?`](#link766f69643f) [`wait-for`](#link776169742d666f72) [`wait-for*`](#link776169742d666f722a) [`wait-for-empty*`](#link776169742d666f722d656d7074792a) [`wait-until`](#link776169742d756e74696c) [`wait-until*`](#link776169742d756e74696c2a) [`warn`](#link7761726e) [`week+`](#link7765656b2b) [`week-of-date`](#link7765656b2d6f662d64617465) [`when`](#link7768656e) [`when-permission`](#link7768656e2d7065726d697373696f6e) [`while`](#link7768696c65) [`window-fixed-size?`](#link77696e646f772d66697865642d73697a653f) [`window-full-screen?`](#link77696e646f772d66756c6c2d73637265656e3f) [`window-padded?`](#link77696e646f772d7061646465643f) [`with-colors`](#link776974682d636f6c6f7273) [`with-error-handler`](#link776974682d6572726f722d68616e646c6572) [`with-final`](#link776974682d66696e616c) [`with-mutex-lock`](#link776974682d6d757465782d6c6f636b) [`with-mutex-rlock`](#link776974682d6d757465782d726c6f636b) [`wrap-delete-text-grid`](#link777261702d64656c6574652d746578742d67726964) [`wrap-insert-text-grid`](#link777261702d696e736572742d746578742d67726964) [`write`](#link7772697465) [`write-binary`](#link77726974652d62696e617279) [`write-binary-at`](#link77726974652d62696e6172792d6174) [`write-string`](#link77726974652d737472696e67) [`write-zimage`](#link77726974652d7a696d616765) [`year+`](#link796561722b) [`zedit-current-selection-text`](#link7a656469742d63757272656e742d73656c656374696f6e2d74657874) [`zimage-header`](#link7a696d6167652d686561646572) [`zimage-loadable?`](#link7a696d6167652d6c6f616461626c653f) [`zimage-runable?`](#link7a696d6167652d72756e61626c653f) 

# By Topics {#topics}

## Actions {#action}

This section concerns the action class and related functions. Actions can be used as an asynchronous interface to the host system, provided the functions action.start, action.progress, action.result, and action.get-args are defined. These functions serve as callbacks into the Go part and need to be implemented on the Go side by the user of the action system. The host system must find the action initialization code and execute it; this code should call `register-action` to register any actions provided, and then the host system may call `get-action` and `action-start` to execute the action within Lisp. Procedure action-start takes an action and a taskid and performs the action. To make `action-stop` work, you have listen to the 'stop message using `task-recv` and shutdown the action appropriately. While the action runs, periodically call `action.progress`. Call `action.result` once the action ends or if an error occurs that does not allow the action to complete. Use `action.get-args` to obtain the arguments of the action, which must be an array of valid Z3S5 Lisp objects. The host system might e.g. prompt the user for values, or these may depend on selected objects in a GUI interface. The host interfacing functions generally receive the action, and its name as second and the ID symbol as third argument in addition to other arguments. The second and third argument are provided for convenience, since processing a #name string and an #id symbol is much easier for a dispatch function in Go than the #action itself, which is an object instance and internally represented as a complex array.

### action : class

Usage: `(new action <info-clause> <name-clause> <proc-clause> ...) => action`

The action class describes instances of actions that serve as plugins for the system hosting Z3S5 Lisp. Each action has a `name`, `prefix` and `info` string property and a unique `id`. Property `args` is an array that specifies the type of arguments of the action. This may be used by an implementation of action.get-args. The `proc` property must be a function taking the action and a task-id as argument and processing the action sequentially until it is completed or `task-recv` returns the 'stop signal. An action may store the result of computation in the `result` property, an error in the `error` property, and an arbitrary state in the `state` property. After processing or if an error occurs, action.result should be called so the host can process the result or error. The action system requires the implementation of procedures action.start, action.progress, action.get-args, and action.result. These are usually defined in the host system, for example in the Go implementation of an application using Z3S5 Lisp actions, and serve as callback functions from Lisp to Go.

See also: [`action`](#link616374696f6e), [`action-stop`](#link616374696f6e2d73746f70), [`action.start`](#link616374696f6e2e7374617274), [`action.progress`](#link616374696f6e2e70726f6772657373), [`action.get-args`](#link616374696f6e2e6765742d61726773), [`action.result`](#link616374696f6e2e726573756c74).	 [→index](#idx)

### action-start : method

Usage: `(action-start action)`

Start `action`, which runs the action's `proc` in a task with the action and a task-id as argument. The `proc` of the `action` should periodically check for the 'stop signal using `task-recv` if the action should be cancellable, should call action.progress to report progress, action.error in case of an error, and action.result to report the result.

See also: [`action`](#link616374696f6e), [`action-stop`](#link616374696f6e2d73746f70), [`action-start`](#link616374696f6e2d7374617274), [`action.progress`](#link616374696f6e2e70726f6772657373), [`action.get-args`](#link616374696f6e2e6765742d61726773), [`action.result`](#link616374696f6e2e726573756c74).	 [→index](#idx)

### action-stop : method

Usage: `(action-stop action)`

The stop method sends a 'stop signal to the action's running `proc`. It is up to the `proc` to check for the signal using `task-recv` and terminate the action gracefully.

See also: [`action`](#link616374696f6e), [`action-stop`](#link616374696f6e2d73746f70), [`action-start`](#link616374696f6e2d7374617274), [`action.progress`](#link616374696f6e2e70726f6772657373), [`action.get-args`](#link616374696f6e2e6765742d61726773), [`action.result`](#link616374696f6e2e726573756c74).	 [→index](#idx)

### `action.get-args` : procedure/3

Usage: `(action.get-args prefix name id arg-spec) => array`

Used to request an array of arguments for an action with `prefix`, `name` and `id` from the host system, according to the specification given in `arg-spec`, which is usually the same as `argspec.`

See also: [`action`](#link616374696f6e), [`action-stop`](#link616374696f6e2d73746f70), [`action.start`](#link616374696f6e2e7374617274), [`action.progress`](#link616374696f6e2e70726f6772657373), [`action.get-args`](#link616374696f6e2e6765742d61726773), [`action.result`](#link616374696f6e2e726573756c74).	 [→index](#idx)

### `action.progress` : procedure/5

Usage: `(action.progress prefix name id taskid perc msg)`

Used to notify the host system from within a running `proc` that the action with `prefix`, `name`, `id`, and `taskid` is making progress to `perc` (a float between 0 and 1) with a message `msg`. Leave the message string empty if it is not needed. Implemented in the host system in Go, this function may, for instance, display a progress bar to inform an end-user.

See also: [`action`](#link616374696f6e), [`action-stop`](#link616374696f6e2d73746f70), [`action-start`](#link616374696f6e2d7374617274), [`action.start`](#link616374696f6e2e7374617274), [`action.progress`](#link616374696f6e2e70726f6772657373), [`action.get-args`](#link616374696f6e2e6765742d61726773), [`action.result`](#link616374696f6e2e726573756c74).	 [→index](#idx)

### `action.result` : procedure/5

Usage: `(action.result prefix name id taskid result error?)`

Used to notify the host system of the result of an action with `prefix`, `name`, `id`, and `taskid`. The `result` may be of any type, but `error?` needs to be a bool that indicates whether an error has occured. If `error?` is not nil, then the host implementation should interpret `result` as an error message.

See also: [`action`](#link616374696f6e), [`action-stop`](#link616374696f6e2d73746f70), [`action.start`](#link616374696f6e2e7374617274), [`action.progress`](#link616374696f6e2e70726f6772657373), [`action.get-args`](#link616374696f6e2e6765742d61726773), [`action.result`](#link616374696f6e2e726573756c74).	 [→index](#idx)

### `action.start` : procedure/3

Usage: `(action.start prefix name id taskid)`

Used to notify the host system that the action with `prefix`, `name`, `id`, and `taskid` has been started.

See also: [`action`](#link616374696f6e), [`action-stop`](#link616374696f6e2d73746f70), [`action-start`](#link616374696f6e2d7374617274), [`action.start`](#link616374696f6e2e7374617274), [`action.progress`](#link616374696f6e2e70726f6772657373), [`action.get-args`](#link616374696f6e2e6765742d61726773), [`action.result`](#link616374696f6e2e726573756c74).	 [→index](#idx)

### `change-action-prefix` : procedure/2

Usage: `(change-action-prefix id new-prefix) => bool`

Change the prefix of a registered action with given `id`, or change the prefix of action given by `id`, to `new-prefix`. If the operation succeeds, it returns true, otherwise it returns nil.

See also: [`change-all-action-prefixes`](#link6368616e67652d616c6c2d616374696f6e2d7072656669786573), [`rename-action`](#link72656e616d652d616374696f6e), [`get-action`](#link6765742d616374696f6e), [`action?`](#link616374696f6e3f), [`action`](#link616374696f6e).	 [→index](#idx)

### `change-all-action-prefixes` : procedure/2

Usage: `(change-all-action-prefixes old-prefix new-prefix)`

Change the prefixes of all registered actions with `old-prefix` to `new-prefix.`

See also: [`change-action-prefix`](#link6368616e67652d616374696f6e2d707265666978), [`rename-action`](#link72656e616d652d616374696f6e), [`get-action`](#link6765742d616374696f6e), [`register-action`](#link72656769737465722d616374696f6e), [`action?`](#link616374696f6e3f), [`action`](#link616374696f6e).	 [→index](#idx)

### `get-action` : procedure/1

Usage: `(get-action id) => action`

Return a cloned action based on `id` from the action registry. This action can be run using `action-start` and will get its own taskid.

See also: [`action`](#link616374696f6e), [`has-action-system?`](#link6861732d616374696f6e2d73797374656d3f), [`action-start`](#link616374696f6e2d7374617274), [`action-stop`](#link616374696f6e2d73746f70), [`register-action`](#link72656769737465722d616374696f6e).	 [→index](#idx)

### `has-action-system?` : procedure/0

Usage: `(has-action-system?) => bool`

This predicate is true if the action system is available, `false` otherwise.

See also: [`action`](#link616374696f6e), [`init-actions`](#link696e69742d616374696f6e73), [`action-start`](#link616374696f6e2d7374617274), [`action-stop`](#link616374696f6e2d73746f70), [`registered-actions`](#link726567697374657265642d616374696f6e73), [`register-action`](#link72656769737465722d616374696f6e).	 [→index](#idx)

### `has-action?` : procedure/1

Usage: `(has-action? prefix name) => bool`

Return true if an action with the given `prefix` and `name` is registered, nil otherwise. Actions are indexed by id, so this is much slower than using `get-action` to retrieve a registered action by the value of the 'id property.

See also: [`get-action`](#link6765742d616374696f6e), [`action`](#link616374696f6e), [`has-action-system?`](#link6861732d616374696f6e2d73797374656d3f), [`register-action`](#link72656769737465722d616374696f6e).	 [→index](#idx)

### `init-actions` : procedure/0

Usage: `(init-actions)`

Initialize the action system, signals an error if the action system is not available.

See also: [`action`](#link616374696f6e), [`has-action-system?`](#link6861732d616374696f6e2d73797374656d3f), [`action-start`](#link616374696f6e2d7374617274), [`action-stop`](#link616374696f6e2d73746f70).	 [→index](#idx)

### `register-action` : procedure/1

Usage: `(register-action action)`

Register the `action` which makes it available for processing by the host system. Use `get-action` to obtain an action clone that can be started.

See also: [`action`](#link616374696f6e), [`has-action-system?`](#link6861732d616374696f6e2d73797374656d3f), [`action-start`](#link616374696f6e2d7374617274), [`action-stop`](#link616374696f6e2d73746f70).	 [→index](#idx)

### `rename-action` : procedure/2

Usage: `(rename-action id new-name) => bool`

Rename a registered action with given `id`, or rename the action given as `id`, to `new-name`. If the operation succeeds, it returns true, otherwise it returns nil.

See also: [`change-action-prefix`](#link6368616e67652d616374696f6e2d707265666978), [`change-all-action-prefixes`](#link6368616e67652d616c6c2d616374696f6e2d7072656669786573), [`get-action`](#link6765742d616374696f6e), [`has-action?`](#link6861732d616374696f6e3f), [`action`](#link616374696f6e).	 [→index](#idx)





## Arrays {#array}

This section concerns functions related to arrays, which are dynamic indexed sequences of values.

### `array` : procedure/0 or more

Usage: `(array [arg1] ...) => array`

Create an array containing the arguments given to it.

See also: [`array?`](#link61727261793f), [`build-array`](#link6275696c642d6172726179), [`array+`](#link61727261792b).	 [→index](#idx)

### `array+` : procedure/1 or more

Usage: `(array+ array1 ...) => array`

Create a new array that results from concatenating the given arrays in order. This function does not mutate `array1.`

See also: [`array-ref`](#link61727261792d726566), [`array-len`](#link61727261792d6c656e), [`build-array`](#link6275696c642d6172726179), [`array-slice`](#link61727261792d736c696365), [`array`](#link6172726179), [`array-copy`](#link61727261792d636f7079).	 [→index](#idx)

### `array-append` : procedure/2

Usage: `(array-append arr elem) => array`

Append `elem` to the array `arr`. This function is destructive and mutates the array. Use array-copy if you need a copy.

See also: [`array-ref`](#link61727261792d726566), [`array-len`](#link61727261792d6c656e), [`build-array`](#link6275696c642d6172726179), [`array-slice`](#link61727261792d736c696365), [`array`](#link6172726179), [`array-copy`](#link61727261792d636f7079), [`array+`](#link61727261792b).	 [→index](#idx)

### `array-copy` : procedure/1

Usage: `(array-copy arr) => array`

Return a copy of `arr.`

See also: [`array`](#link6172726179), [`array?`](#link61727261793f), [`array-map!`](#link61727261792d6d617021), [`array-pmap!`](#link61727261792d706d617021).	 [→index](#idx)

### `array-exists?` : procedure/2

Usage: `(array-exists? arr pred) => bool`

Return true if `pred` returns true for at least one element in array `arr`, nil otherwise.

See also: [`exists?`](#link6578697374733f), [`forall?`](#link666f72616c6c3f), [`list-exists?`](#link6c6973742d6578697374733f), [`str-exists?`](#link7374722d6578697374733f), [`seq?`](#link7365713f).	 [→index](#idx)

### `array-forall?` : procedure/2

Usage: `(array-forall? arr pred) => bool`

Return true if predicate `pred` returns true for all elements of array `arr`, nil otherwise.

See also: [`foreach`](#link666f7265616368), [`map`](#link6d6170), [`forall?`](#link666f72616c6c3f), [`str-forall?`](#link7374722d666f72616c6c3f), [`list-forall?`](#link6c6973742d666f72616c6c3f), [`exists?`](#link6578697374733f).	 [→index](#idx)

### `array-foreach` : procedure/2

Usage: `(array-foreach arr proc)`

Apply `proc` to each element of array `arr` in order, for the side effects.

See also: [`foreach`](#link666f7265616368), [`list-foreach`](#link6c6973742d666f7265616368), [`map`](#link6d6170).	 [→index](#idx)

### `array-len` : procedure/1

Usage: `(array-len arr) => int`

Return the length of array `arr.`

See also: [`len`](#link6c656e).	 [→index](#idx)

### `array-map!` : procedure/2

Usage: `(array-map! arr proc)`

Traverse array `arr` in unspecified order and apply `proc` to each element. This mutates the array.

See also: [`array-walk`](#link61727261792d77616c6b), [`array-pmap!`](#link61727261792d706d617021), [`array?`](#link61727261793f), [`map`](#link6d6170), [`seq?`](#link7365713f).	 [→index](#idx)

### `array-pmap!` : procedure/2

Usage: `(array-pmap! arr proc)`

Apply `proc` in unspecified order in parallel to array `arr`, mutating the array to contain the value returned by `proc` each time. Because of the calling overhead for parallel execution, for many workloads array-map! might be faster if `proc` is very fast. If `proc` is slow, then array-pmap! may be much faster for large arrays on machines with many cores.

See also: [`array-map!`](#link61727261792d6d617021), [`array-walk`](#link61727261792d77616c6b), [`array?`](#link61727261793f), [`map`](#link6d6170), [`seq?`](#link7365713f).	 [→index](#idx)

### `array-ref` : procedure/1

Usage: `(array-ref arr n) => any`

Return the element of `arr` at index `n`. Arrays are 0-indexed.

See also: [`array?`](#link61727261793f), [`array`](#link6172726179), [`nth`](#link6e7468), [`seq?`](#link7365713f).	 [→index](#idx)

### `array-reverse` : procedure/1

Usage: `(array-reverse arr) => array`

Create a copy of `arr` that reverses the order of all of its elements.

See also: [`reverse`](#link72657665727365), [`list-reverse`](#link6c6973742d72657665727365), [`str-reverse`](#link7374722d72657665727365).	 [→index](#idx)

### `array-set` : procedure/3

Usage: `(array-set arr idx value)`

Set the value at index `idx` in `arr` to `value`. Arrays are 0-indexed. This mutates the array.

See also: [`array?`](#link61727261793f), [`array`](#link6172726179).	 [→index](#idx)

### `array-slice` : procedure/3

Usage: `(array-slice arr low high) => array`

Slice the array `arr` starting from `low` (inclusive) and ending at `high` (exclusive) and return the slice. This function is destrcutive and mutates the slice. Use array-copy if you need a copy.

See also: [`array-ref`](#link61727261792d726566), [`array-len`](#link61727261792d6c656e), [`array-append`](#link61727261792d617070656e64), [`build-array`](#link6275696c642d6172726179), [`array`](#link6172726179), [`array-copy`](#link61727261792d636f7079), [`array+`](#link61727261792b).	 [→index](#idx)

### `array-sort` : procedure/2

Usage: `(array-sort arr proc) => arr`

Destructively sorts array `arr` by using comparison proc `proc`, which takes two arguments and returns true if the first argument is smaller than the second argument, nil otherwise. The array is returned but it is not copied and modified in place by this procedure. The sorting algorithm is not guaranteed to be stable.

See also: [`sort`](#link736f7274).	 [→index](#idx)

### `array-walk` : procedure/2

Usage: `(array-walk arr proc)`

Traverse the array `arr` from first to last element and apply `proc` to each element for side-effects. Function `proc` takes the index and the array element at that index as argument. If `proc` returns nil, then the traversal stops and the index is returned. If `proc` returns non-nil, traversal continues. If `proc` never returns nil, then the index returned is -1. This function does not mutate the array.

See also: [`array-map!`](#link61727261792d6d617021), [`array-pmap!`](#link61727261792d706d617021), [`array?`](#link61727261793f), [`map`](#link6d6170), [`seq?`](#link7365713f).	 [→index](#idx)

### `array?` : procedure/1

Usage: `(array? obj) => bool`

Return true of `obj` is an array, nil otherwise.

See also: [`seq?`](#link7365713f), [`array`](#link6172726179).	 [→index](#idx)

### `build-array` : procedure/2

Usage: `(build-array n init) => array`

Create an array containing `n` elements with initial value `init.`

See also: [`array`](#link6172726179), [`array?`](#link61727261793f), [`array-slice`](#link61727261792d736c696365), [`array-append`](#link61727261792d617070656e64), [`array-copy`](#link61727261792d636f7079), [`array+`](#link61727261792b).	 [→index](#idx)



## Binary Manipulation {#binary}

This section lists functions for manipulating binary data in memory and on disk.

### `bitand` : procedure/2

Usage: `(bitand n m) => int`

Return the bitwise and of integers `n` and `m.`

See also: [`bitxor`](#link626974786f72), [`bitor`](#link6269746f72), [`bitclear`](#link626974636c656172), [`bitshl`](#link62697473686c), [`bitshr`](#link626974736872).	 [→index](#idx)

### `bitclear` : procedure/2

Usage: `(bitclear n m) => int`

Return the bitwise and-not of integers `n` and `m.`

See also: [`bitxor`](#link626974786f72), [`bitand`](#link626974616e64), [`bitor`](#link6269746f72), [`bitshl`](#link62697473686c), [`bitshr`](#link626974736872).	 [→index](#idx)

### `bitor` : procedure/2

Usage: `(bitor n m) => int`

Return the bitwise or of integers `n` and `m.`

See also: [`bitxor`](#link626974786f72), [`bitand`](#link626974616e64), [`bitclear`](#link626974636c656172), [`bitshl`](#link62697473686c), [`bitshr`](#link626974736872).	 [→index](#idx)

### `bitshl` : procedure/2

Usage: `(bitshl n m) => int`

Return the bitwise left shift of `n` by `m.`

See also: [`bitxor`](#link626974786f72), [`bitor`](#link6269746f72), [`bitand`](#link626974616e64), [`bitclear`](#link626974636c656172), [`bitshr`](#link626974736872).	 [→index](#idx)

### `bitshr` : procedure/2

Usage: `(bitshr n m) => int`

Return the bitwise right shift of `n` by `m.`

See also: [`bitxor`](#link626974786f72), [`bitor`](#link6269746f72), [`bitand`](#link626974616e64), [`bitclear`](#link626974636c656172), [`bitshl`](#link62697473686c).	 [→index](#idx)

### `bitxor` : procedure/2

Usage: `(bitxor n m) => int`

Return the bitwise exclusive or value of integers `n` and `m.`

See also: [`bitand`](#link626974616e64), [`bitor`](#link6269746f72), [`bitclear`](#link626974636c656172), [`bitshl`](#link62697473686c), [`bitshr`](#link626974736872).	 [→index](#idx)

### `blob-chksum` : procedure/1 or more

Usage: `(blob-chksum b [start] [end]) => blob`

Return the checksum of the contents of blob `b` as new blob. The checksum is cryptographically secure. If the optional `start` and `end` are provided, then only the bytes from `start` (inclusive) to `end` (exclusive) are checksummed.

See also: [`fchksum`](#link6663686b73756d), [`blob-free`](#link626c6f622d66726565).	 [→index](#idx)

### `blob-equal?` : procedure/2

Usage: `(blob-equal? b1 b2) => bool`

Return true if `b1` and `b2` are equal, nil otherwise. Two blobs are equal if they are either both invalid, both contain no valid data, or their contents contain exactly the same binary data.

See also: [`str->blob`](#link7374722d3e626c6f62), [`blob->str`](#link626c6f622d3e737472), [`blob-free`](#link626c6f622d66726565).	 [→index](#idx)

### `blob-free` : procedure/1

Usage: `(blob-free b)`

Frees the binary data stored in blob `b` and makes the blob invalid.

See also: [`make-blob`](#link6d616b652d626c6f62), [`valid?`](#link76616c69643f), [`str->blob`](#link7374722d3e626c6f62), [`blob->str`](#link626c6f622d3e737472), [`blob-equal?`](#link626c6f622d657175616c3f).	 [→index](#idx)

### `blob?` : procedure/1

Usage: `(blob? obj) => bool`

Return true if `obj` is a binary blob, nil otherwise.

See also: [`blob->ascii85`](#link626c6f622d3e61736369693835), [`blob->base64`](#link626c6f622d3e626173653634), [`blob->hex`](#link626c6f622d3e686578), [`blob->str`](#link626c6f622d3e737472), [`blob-free`](#link626c6f622d66726565), [`blob-chksum`](#link626c6f622d63686b73756d), [`blob-equal?`](#link626c6f622d657175616c3f), [`valid?`](#link76616c69643f).	 [→index](#idx)

### `make-blob` : procedure/1

Usage: `(make-blob n) => blob`

Make a binary blob of size `n` initialized to zeroes.

See also: [`blob-free`](#link626c6f622d66726565), [`valid?`](#link76616c69643f), [`blob-equal?`](#link626c6f622d657175616c3f).	 [→index](#idx)

### `peek` : procedure/4

Usage: `(peek b pos end sel) => num`

Read a numeric value determined by selector `sel` from binary blob `b` at position `pos` with endianness `end`. Possible values for endianness are 'little and 'big, and possible values for `sel` must be one of '(bool int8 uint8 int16 uint16 int32 uint32 int64 uint64 float32 float64).

See also: [`poke`](#link706f6b65), [`read-binary`](#link726561642d62696e617279).	 [→index](#idx)

### `poke` : procedure/5

Usage: `(poke b pos end sel n)`

Write numeric value `n` as type `sel` with endianness `end` into the binary blob `b` at position `pos`. Possible values for endianness are 'little and 'big, and possible values for `sel` must be one of '(bool int8 uint8 int16 uint16 int32 uint32 int64 uint64 float32 float64).

See also: [`peek`](#link7065656b), [`write-binary`](#link77726974652d62696e617279).	 [→index](#idx)



## Boxed Data Structures {#boxed}

Boxed values are used for dealing with foreign data structures in Lisp.

### `valid?` : procedure/1

Usage: `(valid? obj) => bool`

Return true if `obj` is a valid object, nil otherwise. What exactly object validity means is undefined, but certain kind of objects such as graphics objects may be marked invalid when they can no longer be used because they have been disposed off by a subsystem and cannot be automatically garbage collected. Generally, invalid objects ought no longer be used and need to be discarded.

See also: [`blob?`](#link626c6f623f).	 [→index](#idx)















## Concurrency and Parallel Programming {#concurrency}

There are several mechanisms for doing parallel and concurrent programming in Z3S5 Lisp. Synchronization primitives are also listed in this section. Generally, users are advised to remain vigilant about potential race conditions.

### `ccmp` : macro/2

Usage: `(ccmp sym value) => int`

Compare the integer value of `sym` with the integer `value`, return 0 if `sym` = `value`, -1 if `sym` < `value`, and 1 if `sym` > `value`. This operation is synchronized between tasks and futures.

See also: [`cinc!`](#link63696e6321), [`cdec!`](#link6364656321), [`cwait`](#link6377616974), [`cst!`](#link63737421).	 [→index](#idx)

### `cdec!` : macro/1

Usage: `(cdec! sym) => int`

Decrease the integer value stored in top-level symbol `sym` by 1 and return the new value. This operation is synchronized between tasks and futures.

See also: [`cinc!`](#link63696e6321), [`cwait`](#link6377616974), [`ccmp`](#link63636d70), [`cst!`](#link63737421).	 [→index](#idx)

### `cinc!` : macro/1

Usage: `(cinc! sym) => int`

Increase the integer value stored in top-level symbol `sym` by 1 and return the new value. This operation is synchronized between tasks and futures.

See also: [`cdec!`](#link6364656321), [`cwait`](#link6377616974), [`ccmp`](#link63636d70), [`cst!`](#link63737421).	 [→index](#idx)

### `cpunum` : procedure/0

Usage: `(cpunum)`

Return the number of cpu cores of this machine.

See also: [`sys`](#link737973).	 [→index](#idx)

**Warning: This function also counts virtual cores on the emulator. The original Z3S5 machine did not have virtual cpu cores.**

### `cst!` : procedure/2

Usage: `(cst! sym value)`

Set the value of `sym` to integer `value`. This operation is synchronized between tasks and futures.

See also: [`cinc!`](#link63696e6321), [`cdec!`](#link6364656321), [`ccmp`](#link63636d70), [`cwait`](#link6377616974).	 [→index](#idx)

### `cwait` : procedure/3

Usage: `(cwait sym value timeout)`

Wait until integer counter `sym` has `value` or `timeout` milliseconds have passed. If `imeout` is 0, then this routine might wait indefinitely. This operation is synchronized between tasks and futures.

See also: [`cinc!`](#link63696e6321), [`cdec!`](#link6364656321), [`ccmp`](#link63636d70), [`cst!`](#link63737421).	 [→index](#idx)

### `enq` : procedure/1

Usage: `(enq proc)`

Put `proc` on a special internal queue for sequential execution and execute it when able. `proc` must be a prodedure that takes no arguments. The queue can be used to synchronizing i/o commands but special care must be taken that `proc` terminates, or else the system might be damaged.

See also: [`task`](#link7461736b), [`future`](#link667574757265), [`synout`](#link73796e6f7574), [`synouty`](#link73796e6f757479).	 [→index](#idx)

**Warning: Calls to enq can never be nested, neither explicitly or implicitly by calling enq anywhere else in the call chain!**

### `force` : procedure/1

Usage: `(force fut) => any`

Obtain the value of the computation encapsulated by future `fut`, halting the current task until it has been obtained. If the future never ends computation, e.g. in an infinite loop, the program may halt indefinitely.

See also: [`future`](#link667574757265), [`task`](#link7461736b), [`make-mutex`](#link6d616b652d6d75746578).	 [→index](#idx)

### future : special form

Usage: `(future ...) => future`

Turn the body of this form into a promise for a future value. The body is executed in parallel and the final value can be retrieved by using (force f) on the future returned by this macro.

See also: [`force`](#link666f726365), [`task`](#link7461736b).	 [→index](#idx)

### `make-mutex` : procedure/1

Usage: `(make-mutex) => mutex`

Create a new mutex.

See also: [`mutex-lock`](#link6d757465782d6c6f636b), [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`mutex-rlock`](#link6d757465782d726c6f636b), [`mutex-runlock`](#link6d757465782d72756e6c6f636b).	 [→index](#idx)

### `mutex-lock` : procedure/1

Usage: `(mutex-lock m)`

Lock the mutex `m` for writing. This may halt the current task until the mutex has been unlocked by another task.

See also: [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`make-mutex`](#link6d616b652d6d75746578), [`mutex-rlock`](#link6d757465782d726c6f636b), [`mutex-runlock`](#link6d757465782d72756e6c6f636b).	 [→index](#idx)

### `mutex-rlock` : procedure/1

Usage: `(mutex-rlock m)`

Lock the mutex `m` for reading. This will allow other tasks to read from it, too, but may block if another task is currently locking it for writing.

See also: [`mutex-runlock`](#link6d757465782d72756e6c6f636b), [`mutex-lock`](#link6d757465782d6c6f636b), [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`make-mutex`](#link6d616b652d6d75746578).	 [→index](#idx)

### `mutex-runlock` : procedure/1

Usage: `(mutex-runlock m)`

Unlock the mutex `m` from reading.

See also: [`mutex-lock`](#link6d757465782d6c6f636b), [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`mutex-rlock`](#link6d757465782d726c6f636b), [`make-mutex`](#link6d616b652d6d75746578).	 [→index](#idx)

### `mutex-unlock` : procedure/1

Usage: `(mutex-unlock m)`

Unlock the mutex `m` for writing. This releases ownership of the mutex and allows other tasks to lock it for writing.

See also: [`mutex-lock`](#link6d757465782d6c6f636b), [`make-mutex`](#link6d616b652d6d75746578), [`mutex-rlock`](#link6d757465782d726c6f636b), [`mutex-runlock`](#link6d757465782d72756e6c6f636b).	 [→index](#idx)

### `prune-task-table` : procedure/0

Usage: `(prune-task-table)`

Remove tasks that are finished from the task table. This includes tasks for which an error has occurred.

See also: [`task-remove`](#link7461736b2d72656d6f7665), [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e).	 [→index](#idx)

### `run-at` : procedure/2

Usage: `(run-at date repeater proc) => int`

Run procedure `proc` with no arguments as task periodically according to the specification in `spec` and return the task ID for the periodic task. Herbey, `date` is either a datetime specification or one of '(now skip next-minute next-quarter next-halfhour next-hour in-2-hours in-3-hours tomorrow next-week next-month next-year), and `repeater` is nil or a procedure that takes a task ID and unix-epoch-nanoseconds and yields a new unix-epoch-nanoseconds value for the next time the procedure shall be run. While the other names are self-explanatory, the 'skip specification means that the task is not run immediately but rather that it is first run at (repeater -1 (now)). Timing resolution for the scheduler is about 1 minute. Consider using interrupts for periodic events with smaller time resolutions. The scheduler uses relative intervals and has 'drift'.

See also: [`task`](#link7461736b), [`task-send`](#link7461736b2d73656e64).	 [→index](#idx)

**Warning: Tasks scheduled by run-at are not persistent! They are only run until the system is shutdown.**

### systask : special form

Usage: `(systask body ...)`

Evaluate the expressions of `body` in parallel in a system task, which is similar to a future but cannot be forced.

See also: [`future`](#link667574757265), [`task`](#link7461736b).	 [→index](#idx)

### `task` : procedure/1

Usage: `(task sel proc) => int`

Create a new task for concurrently running `proc`, a procedure that takes its own ID as argument. The `sel` argument must be a symbol in '(auto manual remove). If `sel` is 'remove, then the task is always removed from the task table after it has finished, even if an error has occurred. If sel is 'auto, then the task is removed from the task table if it ends without producing an error. If `sel` is 'manual then the task is not removed from the task table, its state is either 'canceled, 'finished, or 'error, and it and must be removed manually with `task-remove` or `prune-task-table`. Broadcast messages are never removed. Tasks are more heavy-weight than futures and allow for message-passing.

See also: [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e), [`task-state`](#link7461736b2d7374617465), [`task-broadcast`](#link7461736b2d62726f616463617374), [`task-send`](#link7461736b2d73656e64), [`task-recv`](#link7461736b2d72656376), [`task-remove`](#link7461736b2d72656d6f7665), [`prune-task-table`](#link7072756e652d7461736b2d7461626c65).	 [→index](#idx)

### `task-broadcast` : procedure/2

Usage: `(task-broadcast id msg)`

Send a message from task `id` to the blackboard. Tasks automatically send the message 'finished to the blackboard when they are finished.

See also: [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e), [`task-state`](#link7461736b2d7374617465), [`task-send`](#link7461736b2d73656e64), [`task-recv`](#link7461736b2d72656376).	 [→index](#idx)

### `task-recv` : procedure/1

Usage: `(task-recv id) => any`

Receive a message for task `id`, or nil if there is no message. This is typically used by the task with `id` itself to periodically check for new messages while doing other work. By convention, if a task receives the message 'end it ought to terminate at the next convenient occasion, whereas upon receiving 'cancel it ought to terminate in an expedited manner.

See also: [`task-send`](#link7461736b2d73656e64), [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e), [`task-state`](#link7461736b2d7374617465), [`task-broadcast`](#link7461736b2d62726f616463617374).	 [→index](#idx)

**Warning: Busy polling for new messages in a tight loop is inefficient and ought to be avoided.**

### `task-remove` : procedure/1

Usage: `(task-remove id)`

Remove task `id` from the task table. The task can no longer be interacted with.

See also: [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-state`](#link7461736b2d7374617465).	 [→index](#idx)

### `task-run` : procedure/1

Usage: `(task-run id)`

Run task `id`, which must have been previously created with task. Attempting to run a task that is already running results in an error unless `silent?` is true. If silent? is true, the function does never produce an error.

See also: [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-state`](#link7461736b2d7374617465), [`task-send`](#link7461736b2d73656e64), [`task-recv`](#link7461736b2d72656376), [`task-broadcast-`](#link7461736b2d62726f6164636173742d).	 [→index](#idx)

### `task-schedule` : procedure/1

Usage: `(task-schedule sel id)`

Schedule task `id` for running, starting it as soon as other tasks have finished. The scheduler attempts to avoid running more than (cpunum) tasks at once.

See also: [`task`](#link7461736b), [`task-run`](#link7461736b2d72756e).	 [→index](#idx)

### `task-send` : procedure/2

Usage: `(task-send id msg)`

Send a message `msg` to task `id`. The task needs to cooperatively use task-recv to reply to the message. It is up to the receiving task what to do with the message once it has been received, or how often to check for new messages.

See also: [`task-broadcast`](#link7461736b2d62726f616463617374), [`task-recv`](#link7461736b2d72656376), [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e), [`task-state`](#link7461736b2d7374617465).	 [→index](#idx)

### `task-state` : procedure/1

Usage: `(task-state id) => sym`

Return the state of the task, which is a symbol in '(finished error stopped new waiting running).

See also: [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e), [`task-broadcast`](#link7461736b2d62726f616463617374), [`task-recv`](#link7461736b2d72656376), [`task-send`](#link7461736b2d73656e64).	 [→index](#idx)

### `task?` : procedure/1

Usage: `(task? id) => bool`

Check whether the given `id` is for a valid task, return true if it is valid, nil otherwise.

See also: [`task`](#link7461736b), [`task-run`](#link7461736b2d72756e), [`task-state`](#link7461736b2d7374617465), [`task-broadcast`](#link7461736b2d62726f616463617374), [`task-send`](#link7461736b2d73656e64), [`task-recv`](#link7461736b2d72656376).	 [→index](#idx)

### `wait-for` : procedure/2

Usage: `(wait-for dict key)`

Block execution until the value for `key` in `dict` is not-nil. This function may wait indefinitely if no other thread sets the value for `key` to not-nil.

See also: [`wait-for*`](#link776169742d666f722a), [`future`](#link667574757265), [`force`](#link666f726365), [`wait-until`](#link776169742d756e74696c), [`wait-until*`](#link776169742d756e74696c2a).	 [→index](#idx)

**Warning: This cannot be used for synchronization of multiple tasks due to potential race-conditions.**

### `wait-for*` : procedure/3

Usage: `(wait-for* dict key timeout)`

Blocks execution until the value for `key` in `dict` is not-nil or `timeout` nanoseconds have passed, and returns that value or nil if waiting timed out. If `timeout` is negative, then the function waits potentially indefinitely without any timeout. If a non-nil key is not found, the function sleeps at least *sync-wait-lower-bound* nanoseconds and up to *sync-wait-upper-bound* nanoseconds until it looks for the key again.

See also: [`future`](#link667574757265), [`force`](#link666f726365), [`wait-for`](#link776169742d666f72), [`wait-until`](#link776169742d756e74696c), [`wait-until*`](#link776169742d756e74696c2a).	 [→index](#idx)

**Warning: This cannot be used for synchronization of multiple tasks due to potential race-conditions.**

### `wait-for-empty*` : procedure/3

Usage: `(wait-for-empty* dict key timeout)`

Blocks execution until the `key` is no longer present in `dict` or `timeout` nanoseconds have passed. If `timeout` is negative, then the function waits potentially indefinitely without any timeout.

See also: [`future`](#link667574757265), [`force`](#link666f726365), [`wait-for`](#link776169742d666f72), [`wait-until`](#link776169742d756e74696c), [`wait-until*`](#link776169742d756e74696c2a).	 [→index](#idx)

**Warning: This cannot be used for synchronization of multiple tasks due to potential race-conditions.**

### `wait-until` : procedure/3

Usage: `(wait-until dict key pred)`

Blocks execution until the unary predicate `pred` returns true for the value at `key` in `dict`. This function may wait indefinitely if no other thread sets the value in such a way that `pred` returns true when applied to it.

See also: [`wait-for`](#link776169742d666f72), [`future`](#link667574757265), [`force`](#link666f726365), [`wait-until*`](#link776169742d756e74696c2a).	 [→index](#idx)

**Warning: This cannot be used for synchronization of multiple tasks due to potential race-conditions.**

### `wait-until*` : procedure/4

Usage: `(wait-until* dict key pred timeout)`

Blocks execution until the unary predicate `pred` returns true for the value at `key` in `dict`, or `timeout` nanoseconds have passed, and returns the value or nil if waiting timed out. If `timeout` is negative, then the function waits potentially indefinitely without any timeout. If a non-nil key is not found, the function sleeps at least *sync-wait-lower-bound* nanoseconds and up to *sync-wait-upper-bound* nanoseconds until it looks for the key again.

See also: [`future`](#link667574757265), [`force`](#link666f726365), [`wait-for`](#link776169742d666f72), [`wait-until*`](#link776169742d756e74696c2a), [`wait-until`](#link776169742d756e74696c).	 [→index](#idx)

**Warning: This cannot be used for synchronization of multiple tasks due to potential race-conditions.**

### `with-mutex-rlock` : macro/1 or more

Usage: `(with-mutex-rlock m ...) => any`

Execute the body with mutex `m` locked for reading and unlock the mutex afterwards.

See also: [`with-mutex-lock`](#link776974682d6d757465782d6c6f636b), [`make-mutex`](#link6d616b652d6d75746578), [`mutex-lock`](#link6d757465782d6c6f636b), [`mutex-rlock`](#link6d757465782d726c6f636b), [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`mutex-runlock`](#link6d757465782d72756e6c6f636b).	 [→index](#idx)



## Console Input & Output {#console}

These functions access the operating system console (terminal) mostly for string output.

### `nl` : procedure/0

Usage: `(nl)`

Display a newline, advancing the cursor to the next line.

See also: [`out`](#link6f7574), [`outy`](#link6f757479), [`output-at`](#link6f75747075742d6174).	 [→index](#idx)

### `prin1` : procedure/1

Usage: `(prin1 s)`

Print `s` to the host OS terminal, where strings are quoted.

See also: [`princ`](#link7072696e63), [`terpri`](#link746572707269), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx)

### `princ` : procedure/1

Usage: `(princ s)`

Print `s` to the host OS terminal without quoting strings.

See also: [`prin1`](#link7072696e31), [`terpri`](#link746572707269), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx)

### `print` : procedure/1

Usage: `(print x)`

Output `x` on the host OS console and end it with a newline.

See also: [`prin1`](#link7072696e31), [`princ`](#link7072696e63).	 [→index](#idx)

### `terpri` : procedure/0

Usage: `(terpri)`

Advance the host OS terminal to the next line.

See also: [`princ`](#link7072696e63), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx)





## Data Type Conversion {#conversion}

This section lists various ways in which one data type can be converted to another.

### `alist->dict` : procedure/1

Usage: `(alist->dict li) => dict`

Convert an association list `li` into a dictionary. Note that the value will be the cdr of each list element, not the second element, so you need to use an alist with proper pairs '(a . b) if you want b to be a single value.

See also: [`dict->alist`](#link646963742d3e616c697374), [`dict`](#link64696374), [`dict->list`](#link646963742d3e6c697374), [`list->dict`](#link6c6973742d3e64696374).	 [→index](#idx)

### `array->list` : procedure/1

Usage: `(array->list arr) => li`

Convert array `arr` into a list.

See also: [`list->array`](#link6c6973742d3e6172726179), [`array`](#link6172726179).	 [→index](#idx)

### `array->str` : procedure/1

Usage: `(array-str arr) => s`

Convert an array of unicode glyphs as integer values into a string. If the given sequence is not a valid UTF-8 sequence, an error is thrown.

See also: [`str->array`](#link7374722d3e6172726179).	 [→index](#idx)

### `ascii85->blob` : procedure/1

Usage: `(ascii85->blob str) => blob`

Convert the ascii85 encoded string `str` to a binary blob. This will raise an error if `str` is not a valid ascii85 encoded string.

See also: [`blob->ascii85`](#link626c6f622d3e61736369693835), [`base64->blob`](#link6261736536342d3e626c6f62), [`str->blob`](#link7374722d3e626c6f62), [`hex->blob`](#link6865782d3e626c6f62).	 [→index](#idx)

### `base64->blob` : procedure/1

Usage: `(base64->blob str) => blob`

Convert the base64 encoded string `str` to a binary blob. This will raise an error if `str` is not a valid base64 encoded string.

See also: [`blob->base64`](#link626c6f622d3e626173653634), [`hex->blob`](#link6865782d3e626c6f62), [`ascii85->blob`](#link617363696938352d3e626c6f62), [`str->blob`](#link7374722d3e626c6f62).	 [→index](#idx)

### `blob->ascii85` : procedure/1 or more

Usage: `(blob->ascii85 b [start] [end]) => str`

Convert the blob `b` to an ascii85 encoded string. If the optional `start` and `end` are provided, then only bytes from `start` (inclusive) to `end` (exclusive) are converted.

See also: [`blob->hex`](#link626c6f622d3e686578), [`blob->str`](#link626c6f622d3e737472), [`blob->base64`](#link626c6f622d3e626173653634), [`valid?`](#link76616c69643f), [`blob?`](#link626c6f623f).	 [→index](#idx)

### `blob->base64` : procedure/1 or more

Usage: `(blob->base64 b [start] [end]) => str`

Convert the blob `b` to a base64 encoded string. If the optional `start` and `end` are provided, then only bytes from `start` (inclusive) to `end` (exclusive) are converted.

See also: [`base64->blob`](#link6261736536342d3e626c6f62), [`valid?`](#link76616c69643f), [`blob?`](#link626c6f623f), [`blob->str`](#link626c6f622d3e737472), [`blob->hex`](#link626c6f622d3e686578), [`blob->ascii85`](#link626c6f622d3e61736369693835).	 [→index](#idx)

### `blob->hex` : procedure/1 or more

Usage: `(blob->hex b [start] [end]) => str`

Convert the blob `b` to a hexadecimal string of byte values. If the optional `start` and `end` are provided, then only bytes from `start` (inclusive) to `end` (exclusive) are converted.

See also: [`hex->blob`](#link6865782d3e626c6f62), [`str->blob`](#link7374722d3e626c6f62), [`valid?`](#link76616c69643f), [`blob?`](#link626c6f623f), [`blob->base64`](#link626c6f622d3e626173653634), [`blob->ascii85`](#link626c6f622d3e61736369693835).	 [→index](#idx)

### `blob->str` : procedure/1 or more

Usage: `(blob->str b [start] [end]) => str`

Convert blob `b` into a string. Notice that the string may contain binary data that is not suitable for displaying and does not represent valid UTF-8 glyphs. If the optional `start` and `end` are provided, then only bytes from `start` (inclusive) to `end` (exclusive) are converted.

See also: [`str->blob`](#link7374722d3e626c6f62), [`valid?`](#link76616c69643f), [`blob?`](#link626c6f623f).	 [→index](#idx)

### `char->str` : procedure/1

Usage: `(char->str n) => str`

Return a string containing the unicode char based on integer `n.`

See also: [`str->char`](#link7374722d3e63686172).	 [→index](#idx)

### `chars->str` : procedure/1

Usage: `(chars->str a) => str`

Convert an array of UTF-8 rune integers `a` into a UTF-8 encoded string.

See also: [`str->runes`](#link7374722d3e72756e6573), [`str->char`](#link7374722d3e63686172), [`char->str`](#link636861722d3e737472).	 [→index](#idx)

### `dict->alist` : procedure/1

Usage: `(dict->alist d) => li`

Convert a dictionary into an association list. Note that the resulting alist will be a set of proper pairs of the form '(a . b) if the values in the dictionary are not lists.

See also: [`dict`](#link64696374), [`dict-map`](#link646963742d6d6170), [`dict->list`](#link646963742d3e6c697374).	 [→index](#idx)

### `dict->array` : procedure/1

Usage: `(dict-array d) => array`

Return an array that contains all key, value pairs of `d`. A key comes directly before its value, but otherwise the order is unspecified.

See also: [`dict->list`](#link646963742d3e6c697374), [`dict`](#link64696374).	 [→index](#idx)

### `dict->keys` : procedure/1

Usage: `(dict->keys d) => li`

Return the keys of dictionary `d` in arbitrary order.

See also: [`dict`](#link64696374), [`dict->values`](#link646963742d3e76616c756573), [`dict->alist`](#link646963742d3e616c697374), [`dict->list`](#link646963742d3e6c697374).	 [→index](#idx)

### `dict->list` : procedure/1

Usage: `(dict->list d) => li`

Return a list of the form '(key1 value1 key2 value2 ...), where the order of key, value pairs is unspecified.

See also: [`dict->array`](#link646963742d3e6172726179), [`dict`](#link64696374).	 [→index](#idx)

### `dict->values` : procedure/1

Usage: `(dict->values d) => li`

Return the values of dictionary `d` in arbitrary order.

See also: [`dict`](#link64696374), [`dict->keys`](#link646963742d3e6b657973), [`dict->alist`](#link646963742d3e616c697374), [`dict->list`](#link646963742d3e6c697374).	 [→index](#idx)

### `expr->str` : procedure/1

Usage: `(expr->str expr) => str`

Convert a Lisp expression `expr` into a string. Does not use a stream port.

See also: [`str->expr`](#link7374722d3e65787072), [`str->expr*`](#link7374722d3e657870722a), [`openstr`](#link6f70656e737472), [`internalize`](#link696e7465726e616c697a65), [`externalize`](#link65787465726e616c697a65).	 [→index](#idx)

### `hex->blob` : procedure/1

Usage: `(hex->blob str) => blob`

Convert hex string `str` to a blob. This will raise an error if `str` is not a valid hex string.

See also: [`blob->hex`](#link626c6f622d3e686578), [`base64->blob`](#link6261736536342d3e626c6f62), [`ascii85->blob`](#link617363696938352d3e626c6f62), [`str->blob`](#link7374722d3e626c6f62).	 [→index](#idx)

### `list->array` : procedure/1

Usage: `(list->array li) => array`

Convert the list `li` to an array.

See also: [`list`](#link6c697374), [`array`](#link6172726179), [`string`](#link737472696e67), [`nth`](#link6e7468), [`seq?`](#link7365713f).	 [→index](#idx)

### `list->set` : procedure/1

Usage: `(list->set li) => dict`

Create a dict containing true for each element of list `li.`

See also: [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty`](#link7365742d656d707479).	 [→index](#idx)

### `list->str` : procedure/1

Usage: `(list->str li) => string`

Return the string that is composed out of the chars in list `li.`

See also: [`array->str`](#link61727261792d3e737472), [`str->list`](#link7374722d3e6c697374), [`chars`](#link6368617273).	 [→index](#idx)

### `set->list` : procedure/1

Usage: `(set->list s) => li`

Convert set `s` to a list of set elements.

See also: [`list->set`](#link6c6973742d3e736574), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty`](#link7365742d656d707479).	 [→index](#idx)

### `str->array` : procedure/1

Usage: `(str->array s) => array`

Return the string `s` as an array of unicode glyph integer values.

See also: [`array->str`](#link61727261792d3e737472).	 [→index](#idx)

### `str->blob` : procedure/1

Usage: `(str->blob s) => blob`

Convert string `s` into a blob.

See also: [`blob->str`](#link626c6f622d3e737472).	 [→index](#idx)

### `str->char` : procedure/1

Usage: `(str->char s)`

Return the first character of `s` as unicode integer.

See also: [`char->str`](#link636861722d3e737472).	 [→index](#idx)

### `str->chars` : procedure/1

Usage: `(str->chars s) => array`

Convert the UTF-8 string `s` into an array of UTF-8 rune integers. An error may occur if the string is not a valid UTF-8 string.

See also: [`runes->str`](#link72756e65732d3e737472), [`str->char`](#link7374722d3e63686172), [`char->str`](#link636861722d3e737472).	 [→index](#idx)

### `str->expr` : procedure/0 or more

Usage: `(str->expr s [default]) => any`

Convert a string `s` into a Lisp expression. If `default` is provided, it is returned if an error occurs, otherwise an error is raised.

See also: [`expr->str`](#link657870722d3e737472), [`str->expr*`](#link7374722d3e657870722a), [`openstr`](#link6f70656e737472), [`externalize`](#link65787465726e616c697a65), [`internalize`](#link696e7465726e616c697a65).	 [→index](#idx)

### `str->expr*` : procedure/0 or more

Usage: `(str->expr* s [default]) => li`

Convert a string `s` into a list consisting of the Lisp expressions in `s`. If `default` is provided, then this value is put in the result list whenever an error occurs. Otherwise an error is raised. Notice that it might not always be obvious what expression in `s` triggers an error, since this hinges on the way the internal expession parser works.

See also: [`str->expr`](#link7374722d3e65787072), [`expr->str`](#link657870722d3e737472), [`openstr`](#link6f70656e737472), [`internalize`](#link696e7465726e616c697a65), [`externalize`](#link65787465726e616c697a65).	 [→index](#idx)

### `str->list` : procedure/1

Usage: `(str->list s) => list`

Return the sequence of numeric chars that make up string `s.`

See also: [`str->array`](#link7374722d3e6172726179), [`list->str`](#link6c6973742d3e737472), [`array->str`](#link61727261792d3e737472), [`chars`](#link6368617273).	 [→index](#idx)

### `str->sym` : procedure/1

Usage: `(str->sym s) => sym`

Convert a string into a symbol.

See also: [`sym->str`](#link73796d2d3e737472), [`intern`](#link696e7465726e), [`make-symbol`](#link6d616b652d73796d626f6c).	 [→index](#idx)

### `sym->str` : procedure/1

Usage: `(sym->str sym) => str`

Convert a symbol into a string.

See also: [`str->sym`](#link7374722d3e73796d), [`intern`](#link696e7465726e), [`make-symbol`](#link6d616b652d73796d626f6c).	 [→index](#idx)



## Special Data Structures {#data}

This section lists some more specialized data structures and helper functions for them.

### `chars` : procedure/1

Usage: `(chars str) => dict`

Return a charset based on `str`, i.e., dict with the chars of `str` as keys and true as value.

See also: [`dict`](#link64696374), [`get`](#link676574), [`set`](#link736574), [`contains`](#link636f6e7461696e73).	 [→index](#idx)

### `dequeue!` : macro/1 or more

Usage: `(dequeue! sym [def]) => any`

Get the next element from queue `sym`, which must be the unquoted name of a variable, and return it. If a default `def` is given, then this is returned if the queue is empty, otherwise nil is returned.

See also: [`make-queue`](#link6d616b652d7175657565), [`queue?`](#link71756575653f), [`enqueue!`](#link656e717565756521), [`glance`](#link676c616e6365), [`queue-empty?`](#link71756575652d656d7074793f), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx)

### `enqueue!` : macro/2

Usage: `(enqueue! sym elem)`

Put `elem` in queue `sym`, where `sym` is the unquoted name of a variable.

See also: [`make-queue`](#link6d616b652d7175657565), [`queue?`](#link71756575653f), [`dequeue!`](#link6465717565756521), [`glance`](#link676c616e6365), [`queue-empty?`](#link71756575652d656d7074793f), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx)

### `glance` : procedure/1

Usage: `(glance s [def]) => any`

Peek the next element in a stack or queue without changing the data structure. If default `def` is provided, this is returned in case the stack or queue is empty; otherwise nil is returned.

See also: [`make-queue`](#link6d616b652d7175657565), [`make-stack`](#link6d616b652d737461636b), [`queue?`](#link71756575653f), [`enqueue?`](#link656e71756575653f), [`dequeue?`](#link646571756575653f), [`queue-len`](#link71756575652d6c656e), [`stack-len`](#link737461636b2d6c656e), [`pop!`](#link706f7021), [`push!`](#link7075736821).	 [→index](#idx)

### `inchars` : procedure/2

Usage: `(inchars char chars) => bool`

Return true if char is in the charset chars, nil otherwise.

See also: [`chars`](#link6368617273), [`dict`](#link64696374), [`get`](#link676574), [`set`](#link736574), [`has`](#link686173).	 [→index](#idx)

### `make-queue` : procedure/0

Usage: `(make-queue) => array`

Make a synchronized queue.

See also: [`queue?`](#link71756575653f), [`enqueue!`](#link656e717565756521), [`dequeue!`](#link6465717565756521), [`glance`](#link676c616e6365), [`queue-empty?`](#link71756575652d656d7074793f), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx)

**Warning: Never change the array of a synchronized data structure directly, or your warranty is void!**

### `make-set` : procedure/0 or more

Usage: `(make-set [arg1] ... [argn]) => dict`

Create a dictionary out of arguments `arg1` to `argn` that stores true for very argument.

See also: [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx)

### `make-stack` : procedure/0

Usage: `(make-stack) => array`

Make a synchronized stack.

See also: [`stack?`](#link737461636b3f), [`push!`](#link7075736821), [`pop!`](#link706f7021), [`stack-empty?`](#link737461636b2d656d7074793f), [`stack-len`](#link737461636b2d6c656e), [`glance`](#link676c616e6365).	 [→index](#idx)

**Warning: Never change the array of a synchronized data structure directly, or your warranty is void!**

### `pop!` : macro/1 or more

Usage: `(pop! sym [def]) => any`

Get the next element from stack `sym`, which must be the unquoted name of a variable, and return it. If a default `def` is given, then this is returned if the queue is empty, otherwise nil is returned.

See also: [`make-stack`](#link6d616b652d737461636b), [`stack?`](#link737461636b3f), [`push!`](#link7075736821), [`stack-len`](#link737461636b2d6c656e), [`stack-empty?`](#link737461636b2d656d7074793f), [`glance`](#link676c616e6365).	 [→index](#idx)

### `push!` : macro/2

Usage: `(push! sym elem)`

Put `elem` in stack `sym`, where `sym` is the unquoted name of a variable.

See also: [`make-stack`](#link6d616b652d737461636b), [`stack?`](#link737461636b3f), [`pop!`](#link706f7021), [`stack-len`](#link737461636b2d6c656e), [`stack-empty?`](#link737461636b2d656d7074793f), [`glance`](#link676c616e6365).	 [→index](#idx)

### `queue-empty?` : procedure/1

Usage: `(queue-empty? q) => bool`

Return true if the queue `q` is empty, nil otherwise.

See also: [`make-queue`](#link6d616b652d7175657565), [`queue?`](#link71756575653f), [`enqueue!`](#link656e717565756521), [`dequeue!`](#link6465717565756521), [`glance`](#link676c616e6365), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx)

### `queue-len` : procedure/1

Usage: `(queue-len q) => int`

Return the length of the queue `q.`

See also: [`make-queue`](#link6d616b652d7175657565), [`queue?`](#link71756575653f), [`enqueue!`](#link656e717565756521), [`dequeue!`](#link6465717565756521), [`glance`](#link676c616e6365), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx)

**Warning: Be advised that this is of limited use in some concurrent contexts, since the length of the queue might have changed already once you've obtained it!**

### `queue?` : procedure/1

Usage: `(queue? q) => bool`

Return true if `q` is a queue, nil otherwise.

See also: [`make-queue`](#link6d616b652d7175657565), [`enqueue!`](#link656e717565756521), [`dequeue`](#link64657175657565), [`glance`](#link676c616e6365), [`queue-empty?`](#link71756575652d656d7074793f), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx)

### `set-complement` : procedure/2

Usage: `(set-complement a domain) => set`

Return all elements in `domain` that are not elements of `a.`

See also: [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-difference`](#link7365742d646966666572656e6365), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f), [`set-subset?`](#link7365742d7375627365743f), [`set-equal?`](#link7365742d657175616c3f).	 [→index](#idx)

### `set-difference` : procedure/2

Usage: `(set-difference a b) => set`

Return the set-theoretic difference of set `a` minus set `b`, i.e., all elements in `a` that are not in `b.`

See also: [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f), [`set-subset?`](#link7365742d7375627365743f), [`set-equal?`](#link7365742d657175616c3f).	 [→index](#idx)

### `set-element?` : procedure/2

Usage: `(set-element? s elem) => bool`

Return true if set `s` has element `elem`, nil otherwise.

See also: [`make-set`](#link6d616b652d736574), [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx)

### `set-empty?` : procedure/1

Usage: `(set-empty? s) => bool`

Return true if set `s` is empty, nil otherwise.

See also: [`make-set`](#link6d616b652d736574), [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f).	 [→index](#idx)

### `set-equal?` : procedure/2

Usage: `(set-equal? a b) => bool`

Return true if `a` and `b` contain the same elements.

See also: [`set-subset?`](#link7365742d7375627365743f), [`list->set`](#link6c6973742d3e736574), [`set-element?`](#link7365742d656c656d656e743f), [`set->list`](#link7365742d3e6c697374), [`set-union`](#link7365742d756e696f6e), [`set-difference`](#link7365742d646966666572656e6365), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx)

### `set-intersection` : procedure/2

Usage: `(set-intersection a b) => set`

Return the intersection of sets `a` and `b`, i.e., the set of elements that are both in `a` and in `b.`

See also: [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f), [`set-subset?`](#link7365742d7375627365743f), [`set-equal?`](#link7365742d657175616c3f).	 [→index](#idx)

### `set-subset?` : procedure/2

Usage: `(set-subset? a b) => bool`

Return true if `a` is a subset of `b`, nil otherwise.

See also: [`set-equal?`](#link7365742d657175616c3f), [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-difference`](#link7365742d646966666572656e6365), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx)

### `set-union` : procedure/2

Usage: `(set-union a b) => set`

Return the union of sets `a` and `b` containing all elements that are in `a` or in `b` (or both).

See also: [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx)

### `set?` : procedure/1

Usage: `(set? x) => bool`

Return true if `x` can be used as a set, nil otherwise.

See also: [`list->set`](#link6c6973742d3e736574), [`make-set`](#link6d616b652d736574), [`set->list`](#link7365742d3e6c697374), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx)

### `stack-empty?` : procedure/1

Usage: `(queue-empty? s) => bool`

Return true if the stack `s` is empty, nil otherwise.

See also: [`make-stack`](#link6d616b652d737461636b), [`stack?`](#link737461636b3f), [`push!`](#link7075736821), [`pop!`](#link706f7021), [`stack-len`](#link737461636b2d6c656e), [`glance`](#link676c616e6365).	 [→index](#idx)

### `stack-len` : procedure/1

Usage: `(stack-len s) => int`

Return the length of the stack `s.`

See also: [`make-queue`](#link6d616b652d7175657565), [`queue?`](#link71756575653f), [`enqueue!`](#link656e717565756521), [`dequeue!`](#link6465717565756521), [`glance`](#link676c616e6365), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx)

**Warning: Be advised that this is of limited use in some concurrent contexts, since the length of the queue might have changed already once you've obtained it!**

### `stack?` : procedure/1

Usage: `(stack? q) => bool`

Return true if `q` is a stack, nil otherwise.

See also: [`make-stack`](#link6d616b652d737461636b), [`push!`](#link7075736821), [`pop!`](#link706f7021), [`stack-empty?`](#link737461636b2d656d7074793f), [`stack-len`](#link737461636b2d6c656e), [`glance`](#link676c616e6365).	 [→index](#idx)



## Databases {#db}

These functions allow for Sqlite3 database access. The module needs to be enabled with the "db" build tag. It also provides access to key-value databases with prefix 'kvdb and the automated remember-recall system, both of which are implemented in Z3S5 Lisp on top of the 'db module. To use the remember system, it needs to be initialized first by calling `(init-remember)`.

### `db.blob` : procedure/2

Usage: `(db.blob db-result n) => fl`

Get the content of column `n` in `db-result` as blob. A blob is a boxed memory area holding binary data.

See also: [`db.str`](#link64622e737472).	 [→index](#idx)

### `db.close` : procedure/1

Usage: `(db.close db)`

Close the database `db.`

See also: [`db.open`](#link64622e6f70656e), [`db.open*`](#link64622e6f70656e2a), [`db.exec`](#link64622e65786563), [`db.query`](#link64622e7175657279).	 [→index](#idx)

### `db.close-result` : procedure/1

Usage: `(db.close-result db-result)`

Close the `db-result`. It is invalid afterwards. This should be done to avoid memory leaks after the result has been used.

See also: [`db.reset`](#link64622e7265736574), [`db.step`](#link64622e73746570), [`db.close`](#link64622e636c6f7365).	 [→index](#idx)

### `db.exec` : procedure/2 or more

Usage: `(db.exec db stmt [args] ...)`

Execute the SQL statement `stmt` in database `db`, binding any optional `args` to the open variable slots in it. This function does not return anything, use db.query to execute a query that returns rows as result.

See also: [`db.query`](#link64622e7175657279), [`db.open`](#link64622e6f70656e), [`db.close`](#link64622e636c6f7365), [`db.open*`](#link64622e6f70656e2a).	 [→index](#idx)

### `db.float` : procedure/2

Usage: `(db.float db-result n) => fl`

Get the content of column `n` in `db-result` as float.

See also: [`db.int`](#link64622e696e74), [`db.str`](#link64622e737472).	 [→index](#idx)

### `db.int` : procedure/2

Usage: `(db.int db-result n) => int`

Get the content of column `n` in `db-result` as integer.

See also: [`db.float`](#link64622e666c6f6174), [`db.str`](#link64622e737472), [`db.blob`](#link64622e626c6f62).	 [→index](#idx)

### `db.open` : procedure/1

Usage: `(db.open fi) => db`

Opens an sqlite3 DB or creates a new, empty database at file path `fi.`

See also: [`db.close`](#link64622e636c6f7365), [`db.exec`](#link64622e65786563), [`db.query`](#link64622e7175657279).	 [→index](#idx)

### `db.open*` : procedure/1

Usage: `(db.open* sel) => db`

Open a temporary database if `sel` is 'temp or an in-memory database if `sel` is 'mem.

See also: [`db.open`](#link64622e6f70656e), [`db.close`](#link64622e636c6f7365), [`db.exec`](#link64622e65786563), [`db.query`](#link64622e7175657279).	 [→index](#idx)

### `db.query` : procedure/2 or more

Usage: `(db.query db stmt [args] ...) => db-result`

Query `db` with SQL statement `stmt`, binding any optional `args` to the open variable slots in it. This function returns a `db-result` that can be used to loop through rows with db.step and obtain columns in them using the various accessor methods.

See also: [`db.exec`](#link64622e65786563), [`db.step`](#link64622e73746570), [`db.int`](#link64622e696e74), [`db.cname`](#link64622e636e616d65), [`db.float`](#link64622e666c6f6174), [`db.str`](#link64622e737472), [`db.expr`](#link64622e65787072), [`db.blob`](#link64622e626c6f62).	 [→index](#idx)

### `db.result-column-count` : procedure/1

Usage: `(db.result-column-count db-result) => int`

Get the number of columns in the rows of `db-result.`

See also: [`db.result-columns`](#link64622e726573756c742d636f6c756d6e73).	 [→index](#idx)

### `db.result-columns` : procedure/1

Usage: `(db.result-columns db-result) => li`

Get a list of column specifications for `db-result`, each consisting of a list with the column name and the column type as string, as these were provided to the query. Since queries support automatic type conversions, this need not reflect the column types in the database schema.

See also: [`db.result-column-count`](#link64622e726573756c742d636f6c756d6e2d636f756e74).	 [→index](#idx)

### `db.row` : procedure/1

Usage: `(db.row db-result) => li`

Return all columns of the current row in `db-result` as list. They have the respective base types INT, FLOAT, BLOB, and TEXT.

See also: [`db.rows`](#link64622e726f7773).	 [→index](#idx)

### `db.step` : procedure/1

Usage: `(db.step db-result) => bool`

Obtain the next result row in `db-result` and return true, or return nil of there is no more row in the result.

See also: [`db.query`](#link64622e7175657279), [`db.row`](#link64622e726f77), [`db.rows`](#link64622e726f7773).	 [→index](#idx)

### `db.str` : procedure/2

Usage: `(db.str db-result n) => str`

Get the content of column `n` in `db-result` as string.

See also: [`db.blob`](#link64622e626c6f62), [`db.int`](#link64622e696e74), [`db.float`](#link64622e666c6f6174).	 [→index](#idx)

### `forget` : procedure/1

Usage: `(forget key)`

Forget the value associated with `key`. This permanently deletes the value from the persistent record.

See also: [`remember`](#link72656d656d626572), [`recall`](#link726563616c6c), [`recollect`](#link7265636f6c6c656374), [`recall-when`](#link726563616c6c2d7768656e), [`recall-info`](#link726563616c6c2d696e666f).	 [→index](#idx)

### `init-remember` : procedure/0

Usage: `(init-remember)`

Initialize the remember database. This requires the modules 'kvdb and 'db enabled. The database is located at (str+ (sysdir 'z3s5-data) "/remembered.z3kv").

See also: [`remember`](#link72656d656d626572), [`recall-when`](#link726563616c6c2d7768656e), [`recall`](#link726563616c6c), [`forget`](#link666f72676574).	 [→index](#idx)

### `kvdb.begin` : procedure/1

Usage: `(kvdb.begin db)`

Begin a key-value database transaction. This can be committed by using kvdb.commit and rolled back by kvdb.rollback.

See also: [`kvdb.comit`](#link6b7664622e636f6d6974), [`kvdb.rollback`](#link6b7664622e726f6c6c6261636b).	 [→index](#idx)

**Warning: Transactions in key-value databases cannot be nested! You have to ensure that there is only one begin...commit pair.**

### `kvdb.close` : procedure/1

Usage: `(kvdb.close db)`

Close a key-value db.

See also: [`kvdb.open`](#link6b7664622e6f70656e).	 [→index](#idx)

### `kvdb.commit` : procedure/1

Usage: `(kvdb.commit db)`

Commit the current transaction, making any changes made since the transaction started permanent.

See also: [`kvdb.rollback`](#link6b7664622e726f6c6c6261636b), [`kvdb.begin`](#link6b7664622e626567696e).	 [→index](#idx)

### `kvdb.db?` : procedure/1

Usage: `(kvdb.db? datum) => bool`

Return true if the given datum is a key-value database, nil otherwise.

See also: [`kvdb.open`](#link6b7664622e6f70656e).	 [→index](#idx)

### `kvdb.forget` : procedure/1

Usage: `(kvdb.forget key)`

Forget the value for `key` if there is one.

See also: [`kvdb.set`](#link6b7664622e736574), [`kvdb.get`](#link6b7664622e676574).	 [→index](#idx)

### `kvdb.forget-everything` : procedure/1

Usage: `(kvdb.forget-everything db)`

Erases all data from the given key-value database `db`, irrecoverably loosing ALL data in it.

See also: [`kvdb.forget`](#link6b7664622e666f72676574).	 [→index](#idx)

**Warning: This operation cannot be undone! Data for all types of keys is deleted. Permanent data loss is imminent!**

### `kvdb.get` : procedure/2 or more

Usage: `(kvdb.get db key [other]) => any`

Get the value stored at `key` in the key-value database `db`. If the value is found, it is returned. If the value is not found and `other` is specified, then `other` is returned. If the value is not found and `other` is not specified, then nil is returned.

See also: [`kvdb.set`](#link6b7664622e736574), [`kvdb.when`](#link6b7664622e7768656e), [`kvdb.info`](#link6b7664622e696e666f), [`kvdb.open`](#link6b7664622e6f70656e), [`kvdb.forget`](#link6b7664622e666f72676574), [`kvdb.close`](#link6b7664622e636c6f7365), [`kvdb.search`](#link6b7664622e736561726368), [`remember`](#link72656d656d626572), [`recall`](#link726563616c6c), [`forget`](#link666f72676574).	 [→index](#idx)

### `kvdb.info` : procedure/2 or more

Usage: `(db key [other]) => (str str)`

Return a list containing the info string and its fuzzy variant stored for `key` in `db`, `other` when the value for `key` is not found. The default for `other` is nil.

See also: [`kvdb.get`](#link6b7664622e676574), [`kvdb.when`](#link6b7664622e7768656e).	 [→index](#idx)

### `kvdb.open` : procedure/1 or more

Usage: `(kvdb.open path) => kvdb-array`

Create or open a key-value database at `path.`

See also: [`kvdb.close`](#link6b7664622e636c6f7365).	 [→index](#idx)

### `kvdb.rollback` : procedure/1

Usage: `(kvdb.rollback db)`

Rollback the changes made since the last transaction has been started and return the key-value database to its previous state.

See also: [`kvdb.commit`](#link6b7664622e636f6d6d6974), [`kvdb.begin`](#link6b7664622e626567696e).	 [→index](#idx)

### `kvdb.search` : procedure/2 or more

Usage: `(kvdb.search db s [keytype] [limit] [fuzzer]) => li`

Search the key-value database `db` for search expression string `s` for optional `keytype` and return a list of matching keys. The optional `keytype` may be one of '(all str sym int expr), where the default is 'all for any kind of key. If the optional `limit` is provided, then only `limit` entries are returned. Default limit is kvdb.*default-search-limit*. If `fuzzer` is a function provided, then a fuzzy string search is performed based on applying fuzzer to the search term; default is nil. 

See also: [`kvdb.get`](#link6b7664622e676574).	 [→index](#idx)

### `kvdb.set` : procedure/3 or more

Usage: `(kvdb.set db key value [info] [fuzzer])`

Set the `value` for `key` in key-value database `db`. The optional `info` string contains searchable information about the value that may be retrieved with the search function. The optional `fuzzer` must be a function that takes a string and yields a fuzzy variant of the string that can be used for fuzzy search. If no fuzzer is specified, then the default metaphone algorithm is used. Keys for the database must be externalizable but notice that integer keys may provide faster performance.

See also: [`kvdb.get`](#link6b7664622e676574), [`kvdb.forget`](#link6b7664622e666f72676574), [`kvdb.open`](#link6b7664622e6f70656e), [`kvdb.close`](#link6b7664622e636c6f7365), [`kvdb.search`](#link6b7664622e736561726368).	 [→index](#idx)

### `kvdb.when` : procedure/2 or more

Usage: `(kvdb.when db key [other]) => str`

Get the date in `db` when the entry for `key` was last modified as a date string. If there is no entry for `key`, then `other` is returned. If `other` is not specified and there is no `key`, then nil is returned.

See also: [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`kvdb.get`](#link6b7664622e676574), [`kvdb.info`](#link6b7664622e696e666f).	 [→index](#idx)

### `recall` : procedure/1 or more

Usage: `(recall key [notfound]) => any`

Obtain the value remembered for `key`, `notfound` if it doesn't exist. If `notfound` is not provided, then nil is returned in case the value for `key` doesn't exist.

See also: [`recall-when`](#link726563616c6c2d7768656e), [`recall-info`](#link726563616c6c2d696e666f), [`recollect`](#link7265636f6c6c656374), [`remember`](#link72656d656d626572), [`forget`](#link666f72676574).	 [→index](#idx)

### `recall-info` : procedure/1 or more

Usage: `(recall-info key [notfound]) => (str str)`

Return a list containing the info string and its fuzzy version for a remembered value with the given `key`, `notfound` if no value for `key` was found. The default for `notfound` is nil.

See also: [`recall-when`](#link726563616c6c2d7768656e), [`recall`](#link726563616c6c), [`recall-when`](#link726563616c6c2d7768656e), [`recollect`](#link7265636f6c6c656374), [`remember`](#link72656d656d626572), [`forget`](#link666f72676574).	 [→index](#idx)

### `recall-when` : procedure/1 or more

Usage: `(recall-when key [notfound]) => datestr`

Obtain the date string when the value for `key` was last modified by remember (set), `notfound` if it doesn't exist. If `notfound` is not provided, then nil is returned in case there is no value for `key.`

See also: [`recall`](#link726563616c6c), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`recall-info`](#link726563616c6c2d696e666f), [`remember`](#link72656d656d626572), [`forget`](#link666f72676574).	 [→index](#idx)

### `recollect` : procedure/1 or more

Usage: `(recollect s [keytype] [limit] [fuzzer]) => li`

Search for remembered items based on search query `s` and return a list of matching keys. The optional `keytype` parameter must be one of '(all str sym int expr), where the default is 'all for all kinds of keys. Up to `limit` results are returned, default is kvdb.*default-search-limit*. The optional `fuzzer` procedure takes a word string and yields a 'fuzzy' version of it. If fuzzer is specified and a procedure, then a fuzzy search is performed.

See also: [`kvdb.search`](#link6b7664622e736561726368), [`recall`](#link726563616c6c), [`recall-info`](#link726563616c6c2d696e666f), [`recall-when`](#link726563616c6c2d7768656e), [`remember`](#link72656d656d626572).	 [→index](#idx)

### `remember` : procedure/2

Usage: `(remember key value [info] [fuzzer])`

Persistently remember `value` by given `key`. See kvdb.set for the optional `info` and `fuzzer` arguments.

See also: [`recall`](#link726563616c6c), [`forget`](#link666f72676574), [`kvdb.set`](#link6b7664622e736574), [`recall-when`](#link726563616c6c2d7768656e), [`recall-info`](#link726563616c6c2d696e666f), [`recollect`](#link7265636f6c6c656374).	 [→index](#idx)





## Dictionaries {#dict}

Dictionaries are thread-safe key-value repositories held in memory. They are internally based on hash tables and have fast access.

### `delete` : procedure/2

Usage: `(delete d key)`

Remove the value for `key` in dict `d`. This also removes the key.

See also: [`dict?`](#link646963743f), [`get`](#link676574), [`set`](#link736574).	 [→index](#idx)

### `dict` : procedure/0 or more

Usage: `(dict [li]) => dict`

Create a dictionary. The option `li` must be a list of the form '(key1 value1 key2 value2 ...). Dictionaries are unordered, hence also not sequences. Dictionaries are safe for concurrent access.

See also: [`array`](#link6172726179), [`list`](#link6c697374).	 [→index](#idx)

### `dict-copy` : procedure/1

Usage: `(dict-copy d) => dict`

Return a copy of dict `d.`

See also: [`dict`](#link64696374), [`dict?`](#link646963743f).	 [→index](#idx)

### `dict-empty?` : procedure/1

Usage: `(dict-empty? d) => bool`

Return true if dict `d` is empty, nil otherwise. As crazy as this may sound, this can have O(n) complexity if the dict is not empty, but it is still going to be more efficient than any other method.

See also: [`dict`](#link64696374).	 [→index](#idx)

### `dict-foreach` : procedure/2

Usage: `(dict-foreach d proc)`

Call `proc` for side-effects with the key and value for each key, value pair in dict `d.`

See also: [`dict-map!`](#link646963742d6d617021), [`dict?`](#link646963743f), [`dict`](#link64696374).	 [→index](#idx)

### `dict-map` : procedure/2

Usage: `(dict-map dict proc) => dict`

Returns a copy of `dict` with `proc` applies to each key value pair as aruments. Keys are immutable, so `proc` must take two arguments and return the new value.

See also: [`dict-map!`](#link646963742d6d617021), [`map`](#link6d6170).	 [→index](#idx)

### `dict-map!` : procedure/2

Usage: `(dict-map! d proc)`

Apply procedure `proc` which takes the key and value as arguments to each key, value pair in dict `d` and set the respective value in `d` to the result of `proc`. Keys are not changed.

See also: [`dict`](#link64696374), [`dict?`](#link646963743f), [`dict-foreach`](#link646963742d666f7265616368).	 [→index](#idx)

### `dict-merge` : procedure/2

Usage: `(dict-merge a b) => dict`

Create a new dict that contains all key-value pairs from dicts `a` and `b`. Note that this function is not symmetric. If a key is in both `a` and `b`, then the key value pair in `a` is retained for this key.

See also: [`dict`](#link64696374), [`dict-map`](#link646963742d6d6170), [`dict-map!`](#link646963742d6d617021), [`dict-foreach`](#link646963742d666f7265616368).	 [→index](#idx)

### `dict?` : procedure/1

Usage: `(dict? obj) => bool`

Return true if `obj` is a dict, nil otherwise.

See also: [`dict`](#link64696374).	 [→index](#idx)

### `get` : procedure/2 or more

Usage: `(get dict key [default]) => any`

Get the value for `key` in `dict`, return `default` if there is no value for `key`. If `default` is omitted, then nil is returned. Provide your own default if you want to store nil.

See also: [`dict`](#link64696374), [`dict?`](#link646963743f), [`set`](#link736574).	 [→index](#idx)

### `get-or-set` : procedure/3

Usage: `(get-or-set d key value)`

Get the value for `key` in dict `d` if it already exists, otherwise set it to `value.`

See also: [`dict?`](#link646963743f), [`get`](#link676574), [`set`](#link736574).	 [→index](#idx)

### `getstacked` : procedure/3

Usage: `(getstacked dict key default)`

Get the topmost element from the stack stored at `key` in `dict`. If the stack is empty or no stack is stored at key, then `default` is returned.

See also: [`pushstacked`](#link70757368737461636b6564), [`popstacked`](#link706f70737461636b6564).	 [→index](#idx)

### `has` : procedure/2

Usage: `(has dict key) => bool`

Return true if the dict `dict` contains an entry for `key`, nil otherwise.

See also: [`dict`](#link64696374), [`get`](#link676574), [`set`](#link736574).	 [→index](#idx)

### `has-key?` : procedure/2

Usage: `(has-key? d key) => bool`

Return true if `d` has key `key`, nil otherwise.

See also: [`dict?`](#link646963743f), [`get`](#link676574), [`set`](#link736574), [`delete`](#link64656c657465).	 [→index](#idx)

### `popstacked` : procedure/3

Usage: `(popstacked dict key default)`

Get the topmost element from the stack stored at `key` in `dict` and remove it from the stack. If the stack is empty or no stack is stored at key, then `default` is returned.

See also: [`pushstacked`](#link70757368737461636b6564), [`getstacked`](#link676574737461636b6564).	 [→index](#idx)

### `pushstacked` : procedure/3

Usage: `(pushstacked dict key datum)`

Push `datum` onto the stack maintained under `key` in the `dict.`

See also: [`getstacked`](#link676574737461636b6564), [`popstacked`](#link706f70737461636b6564).	 [→index](#idx)

### `set` : procedure/3

Usage: `(set d key value)`

Set `value` for `key` in dict `d.`

See also: [`dict`](#link64696374), [`get`](#link676574), [`get-or-set`](#link6765742d6f722d736574).	 [→index](#idx)

### `set*` : procedure/2

Usage: `(set* d li)`

Set in dict `d` the keys and values in list `li`. The list `li` must be of the form (key-1 value-1 key-2 value-2 ... key-n value-n). This function may be slightly faster than using individual `set` operations.

See also: [`dict`](#link64696374), [`set`](#link736574).	 [→index](#idx)









## File Input & Output {#fileio}

These functions allow direct access for reading and writing to files. This module requires the `fileio` build tag.

### `close` : procedure/1

Usage: `(close p)`

Close the port `p`. Calling close twice on the same port should be avoided.

See also: [`open`](#link6f70656e), [`stropen`](#link7374726f70656e).	 [→index](#idx)

### `dir` : procedure/1

Usage: `(dir [path]) => li`

Obtain a directory list for `path`. If `path` is not specified, the current working directory is listed.

See also: [`dir?`](#link6469723f), [`open`](#link6f70656e), [`close`](#link636c6f7365), [`read`](#link72656164), [`write`](#link7772697465).	 [→index](#idx)

### `dir?` : procedure/1

Usage: `(dir? path) => bool`

Check if the file at `path` is a directory and return true, nil if the file does not exist or is not a directory.

See also: [`file-exists?`](#link66696c652d6578697374733f), [`dir`](#link646972), [`open`](#link6f70656e), [`close`](#link636c6f7365), [`read`](#link72656164), [`write`](#link7772697465).	 [→index](#idx)

### `fdelete` : procedure/1

Usage: `(fdelete path)`

Removes the file or directory at `path.`

See also: [`file-exists?`](#link66696c652d6578697374733f), [`dir?`](#link6469723f), [`dir`](#link646972).	 [→index](#idx)

**Warning: This function also deletes directories containing files and all of their subdirectories!**

### `file-port?` : procedure/1

Usage: `(file-port? p) => bool`

Return true if `p` is a file port, nil otherwise.

See also: [`port?`](#link706f72743f), [`str-port?`](#link7374722d706f72743f), [`open`](#link6f70656e), [`stropen`](#link7374726f70656e).	 [→index](#idx)

### `open` : procedure/1 or more

Usage: `(open file-path [modes] [permissions]) => int`

Open the file at `file-path` for reading and writing, and return the stream ID. The optional `modes` argument must be a list containing one of '(read write read-write) for read, write, or read-write access respectively, and may contain any of the following symbols: 'append to append to an existing file, 'create for creating the file if it doesn't exist, 'exclusive for exclusive file access, 'truncate for truncating the file if it exists, and 'sync for attempting to sync file access. The optional `permissions` argument must be a numeric value specifying the Unix file permissions of the file. If these are omitted, then default values '(read-write append create) and 0640 are used.

See also: [`stropen`](#link7374726f70656e), [`close`](#link636c6f7365), [`read`](#link72656164), [`write`](#link7772697465).	 [→index](#idx)

### `read` : procedure/1

Usage: `(read p) => any`

Read an expression from input port `p.`

See also: [`input`](#link696e707574), [`write`](#link7772697465).	 [→index](#idx)

### `read-binary` : procedure/3

Usage: `(read-binary p buff n) => int`

Read `n` or less bytes from input port `p` into binary blob `buff`. If `buff` is smaller than `n`, then an error is raised. If less than `n` bytes are available before the end of file is reached, then the amount k of bytes is read into `buff` and k is returned. If the end of file is reached and no byte has been read, then 0 is returned. So to loop through this, read into the buffer and do something with it while the amount of bytes returned is larger than 0.

See also: [`write-binary`](#link77726974652d62696e617279), [`read`](#link72656164), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx)

### `read-string` : procedure/2

Usage: `(read-string p delstr) => str`

Reads a string from port `p` until the single-byte delimiter character in `delstr` is encountered, and returns the string including the delimiter. If the input ends before the delimiter is encountered, it returns the string up until EOF. Notice that if the empty string is returned then the end of file must have been encountered, since otherwise the string would contain the delimiter.

See also: [`read`](#link72656164), [`read-binary`](#link726561642d62696e617279), [`write-string`](#link77726974652d737472696e67), [`write`](#link7772697465), [`read`](#link72656164), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx)

### `str-port?` : procedure/1

Usage: `(str-port? p) => bool`

Return true if `p` is a string port, nil otherwise.

See also: [`port?`](#link706f72743f), [`file-port?`](#link66696c652d706f72743f), [`stropen`](#link7374726f70656e), [`open`](#link6f70656e).	 [→index](#idx)

### `write` : procedure/2

Usage: `(write p datum) => int`

Write `datum` to output port `p` and return the number of bytes written.

See also: [`write-binary`](#link77726974652d62696e617279), [`write-binary-at`](#link77726974652d62696e6172792d6174), [`read`](#link72656164), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx)

### `write-binary` : procedure/4

Usage: `(write-binary p buff n offset) => int`

Write `n` bytes starting at `offset` in binary blob `buff` to the stream port `p`. This function returns the number of bytes actually written.

See also: [`write-binary-at`](#link77726974652d62696e6172792d6174), [`read-binary`](#link726561642d62696e617279), [`write`](#link7772697465), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx)

### `write-binary-at` : procedure/5

Usage: `(write-binary-at p buff n offset fpos) => int`

Write `n` bytes starting at `offset` in binary blob `buff` to the seekable stream port `p` at the stream position `fpos`. If there is not enough data in `p` to overwrite at position `fpos`, then an error is caused and only part of the data might be written. The function returns the number of bytes actually written.

See also: [`read-binary`](#link726561642d62696e617279), [`write-binary`](#link77726974652d62696e617279), [`write`](#link7772697465), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx)

### `write-string` : procedure/2

Usage: `(write-string p s) => int`

Write string `s` to output port `p` and return the number of bytes written. LF are *not* automatically converted to CR LF sequences on windows.

See also: [`write`](#link7772697465), [`write-binary`](#link77726974652d62696e617279), [`write-binary-at`](#link77726974652d62696e6172792d6174), [`read`](#link72656164), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx)



## Floating Point Arithmetics Package {#float}

The package `fl` provides floating point arithmetics functions. They require the given number not to exceed a value that can be held by a 64 bit float in the range 2.2E-308 to 1.7E+308.

### `fl.abs` : procedure/1

Usage: `(fl.abs x) => fl`

Return the absolute value of `x.`

See also: [`float`](#link666c6f6174), [`*`](#link2a).	 [→index](#idx)

### `fl.acos` : procedure/1

Usage: `(fl.acos x) => fl`

Return the arc cosine of `x.`

See also: [`fl.cos`](#link666c2e636f73).	 [→index](#idx)

### `fl.asin` : procedure/1

Usage: `(fl.asin x) => fl`

Return the arc sine of `x.`

See also: [`fl.acos`](#link666c2e61636f73).	 [→index](#idx)

### `fl.asinh` : procedure/1

Usage: `(fl.asinh x) => fl`

Return the inverse hyperbolic sine of `x.`

See also: [`fl.cosh`](#link666c2e636f7368).	 [→index](#idx)

### `fl.atan` : procedure/1

Usage: `(fl.atan x) => fl`

Return the arctangent of `x` in radians.

See also: [`fl.atanh`](#link666c2e6174616e68), [`fl.tan`](#link666c2e74616e).	 [→index](#idx)

### `fl.atan2` : procedure/2

Usage: `(fl.atan2 x y) => fl`

Atan2 returns the arc tangent of `y` / `x`, using the signs of the two to determine the quadrant of the return value.

See also: [`fl.atan`](#link666c2e6174616e).	 [→index](#idx)

### `fl.atanh` : procedure/1

Usage: `(fl.atanh x) => fl`

Return the inverse hyperbolic tangent of `x.`

See also: [`fl.atan`](#link666c2e6174616e).	 [→index](#idx)

### `fl.cbrt` : procedure/1

Usage: `(fl.cbrt x) => fl`

Return the cube root of `x.`

See also: [`fl.sqrt`](#link666c2e73717274).	 [→index](#idx)

### `fl.ceil` : procedure/1

Usage: `(fl.ceil x) => fl`

Round `x` up to the nearest integer, return it as a floating point number.

See also: [`fl.floor`](#link666c2e666c6f6f72), [`truncate`](#link7472756e63617465), [`int`](#link696e74), [`fl.round`](#link666c2e726f756e64), [`fl.trunc`](#link666c2e7472756e63).	 [→index](#idx)

### `fl.cos` : procedure/1

Usage: `(fl.cos x) => fl`

Return the cosine of `x.`

See also: [`fl.sin`](#link666c2e73696e).	 [→index](#idx)

### `fl.cosh` : procedure/1

Usage: `(fl.cosh x) => fl`

Return the hyperbolic cosine of `x.`

See also: [`fl.cos`](#link666c2e636f73).	 [→index](#idx)

### `fl.dim` : procedure/2

Usage: `(fl.dim x y) => fl`

Return the maximum of x, y or 0.

See also: [`max`](#link6d6178).	 [→index](#idx)

### `fl.erf` : procedure/1

Usage: `(fl.erf x) => fl`

Return the result of the error function of `x.`

See also: [`fl.erfc`](#link666c2e65726663), [`fl.dim`](#link666c2e64696d).	 [→index](#idx)

### `fl.erfc` : procedure/1

Usage: `(fl.erfc x) => fl`

Return the result of the complementary error function of `x.`

See also: [`fl.erfcinv`](#link666c2e65726663696e76), [`fl.erf`](#link666c2e657266).	 [→index](#idx)

### `fl.erfcinv` : procedure/1

Usage: `(fl.erfcinv x) => fl`

Return the inverse of (fl.erfc `x`).

See also: [`fl.erfc`](#link666c2e65726663).	 [→index](#idx)

### `fl.erfinv` : procedure/1

Usage: `(fl.erfinv x) => fl`

Return the inverse of (fl.erf `x`).

See also: [`fl.erf`](#link666c2e657266).	 [→index](#idx)

### `fl.exp` : procedure/1

Usage: `(fl.exp x) => fl`

Return e^`x`, the base-e exponential of `x.`

See also: [`fl.exp`](#link666c2e657870).	 [→index](#idx)

### `fl.exp2` : procedure/2

Usage: `(fl.exp2 x) => fl`

Return 2^`x`, the base-2 exponential of `x.`

See also: [`fl.exp`](#link666c2e657870).	 [→index](#idx)

### `fl.expm1` : procedure/1

Usage: `(fl.expm1 x) => fl`

Return e^`x-1`, the base-e exponential of (sub1 `x`). This is more accurate than (sub1 (fl.exp `x`)) when `x` is very small.

See also: [`fl.exp`](#link666c2e657870).	 [→index](#idx)

### `fl.floor` : procedure/1

Usage: `(fl.floor x) => fl`

Return `x` rounded to the nearest integer below as floating point number.

See also: [`fl.ceil`](#link666c2e6365696c), [`truncate`](#link7472756e63617465), [`int`](#link696e74).	 [→index](#idx)

### `fl.fma` : procedure/3

Usage: `(fl.fma x y z) => fl`

Return the fused multiply-add of `x`, `y`, `z`, which is `x` * `y` + `z.`

See also: [`*`](#link2a), [`+`](#link2b).	 [→index](#idx)

### `fl.frexp` : procedure/1

Usage: `(fl.frexp x) => li`

Break `x` into a normalized fraction and an integral power of two. It returns a list of (frac exp) containing a float and an integer satisfying `x` == `frac` × 2^`exp` where the absolute value of `frac` is in the interval [0.5, 1).

See also: [`fl.exp`](#link666c2e657870).	 [→index](#idx)

### `fl.gamma` : procedure/1

Usage: `(fl.gamma x) => fl`

Compute the Gamma function of `x.`

See also: [`fl.lgamma`](#link666c2e6c67616d6d61).	 [→index](#idx)

### `fl.hypot` : procedure/2

Usage: `(fl.hypot x y) => fl`

Compute the square root of x^2 and y^2.

See also: [`fl.sqrt`](#link666c2e73717274).	 [→index](#idx)

### `fl.ilogb` : procedure/1

Usage: `(fl.ilogb x) => fl`

Return the binary exponent of `x` as a floating point number.

See also: [`fl.exp2`](#link666c2e65787032).	 [→index](#idx)

### `fl.inf` : procedure/1

Usage: `(fl.inf x) => fl`

Return positive 64 bit floating point infinity +INF if `x` >= 0 and negative 64 bit floating point finfinity -INF if `x` < 0.

See also: [`fl.is-nan?`](#link666c2e69732d6e616e3f).	 [→index](#idx)

### `fl.is-nan?` : procedure/1

Usage: `(fl.is-nan? x) => bool`

Return true if `x` is not a number according to IEEE 754 floating point arithmetics, nil otherwise.

See also: [`fl.inf`](#link666c2e696e66).	 [→index](#idx)

### `fl.j0` : procedure/1

Usage: `(fl.j0 x) => fl`

Apply the order-zero Bessel function of the first kind to `x.`

See also: [`fl.j1`](#link666c2e6a31), [`fl.jn`](#link666c2e6a6e), [`fl.y0`](#link666c2e7930), [`fl.y1`](#link666c2e7931), [`fl.yn`](#link666c2e796e).	 [→index](#idx)

### `fl.j1` : procedure/1

Usage: `(fl.j1 x) => fl`

Apply the the order-one Bessel function of the first kind `x.`

See also: [`fl.j0`](#link666c2e6a30), [`fl.jn`](#link666c2e6a6e), [`fl.y0`](#link666c2e7930), [`fl.y1`](#link666c2e7931), [`fl.yn`](#link666c2e796e).	 [→index](#idx)

### `fl.jn` : procedure/1

Usage: `(fl.jn n x) => fl`

Apply the Bessel function of order `n` to `x`. The number `n` must be an integer.

See also: [`fl.j1`](#link666c2e6a31), [`fl.j0`](#link666c2e6a30), [`fl.y0`](#link666c2e7930), [`fl.y1`](#link666c2e7931), [`fl.yn`](#link666c2e796e).	 [→index](#idx)

### `fl.ldexp` : procedure/2

Usage: `(fl.ldexp x n) => fl`

Return the inverse of fl.frexp, `x` * 2^`n.`

See also: [`fl.frexp`](#link666c2e6672657870).	 [→index](#idx)

### `fl.lgamma` : procedure/1

Usage: `(fl.lgamma x) => li`

Return a list containing the natural logarithm and sign (-1 or +1) of the Gamma function applied to `x.`

See also: [`fl.gamma`](#link666c2e67616d6d61).	 [→index](#idx)

### `fl.log` : procedure/1

Usage: `(fl.log x) => fl`

Return the natural logarithm of `x.`

See also: [`fl.log10`](#link666c2e6c6f673130), [`fl.log2`](#link666c2e6c6f6732), [`fl.logb`](#link666c2e6c6f6762), [`fl.log1p`](#link666c2e6c6f673170).	 [→index](#idx)

### `fl.log10` : procedure/1

Usage: `(fl.log10 x) => fl`

Return the decimal logarithm of `x.`

See also: [`fl.log`](#link666c2e6c6f67), [`fl.log2`](#link666c2e6c6f6732), [`fl.logb`](#link666c2e6c6f6762), [`fl.log1p`](#link666c2e6c6f673170).	 [→index](#idx)

### `fl.log1p` : procedure/1

Usage: `(fl.log1p x) => fl`

Return the natural logarithm of `x` + 1. This function is more accurate than (fl.log (add1 x)) if `x` is close to 0.

See also: [`fl.log`](#link666c2e6c6f67), [`fl.log2`](#link666c2e6c6f6732), [`fl.logb`](#link666c2e6c6f6762), [`fl.log10`](#link666c2e6c6f673130).	 [→index](#idx)

### `fl.log2` : procedure/1

Usage: `(fl.log2 x) => fl`

Return the binary logarithm of `x`. This is important for calculating entropy, for example.

See also: [`fl.log`](#link666c2e6c6f67), [`fl.log10`](#link666c2e6c6f673130), [`fl.log1p`](#link666c2e6c6f673170), [`fl.logb`](#link666c2e6c6f6762).	 [→index](#idx)

### `fl.logb` : procedure/1

Usage: `(fl.logb x) => fl`

Return the binary exponent of `x.`

See also: [`fl.log`](#link666c2e6c6f67), [`fl.log10`](#link666c2e6c6f673130), [`fl.log1p`](#link666c2e6c6f673170), [`fl.logb`](#link666c2e6c6f6762), [`fl.log2`](#link666c2e6c6f6732).	 [→index](#idx)

### `fl.max` : procedure/2

Usage: `(fl.max x y) => fl`

Return the larger value of two floating point arguments `x` and `y.`

See also: [`fl.min`](#link666c2e6d696e), [`max`](#link6d6178), [`min`](#link6d696e).	 [→index](#idx)

### `fl.min` : procedure/2

Usage: `(fl.min x y) => fl`

Return the smaller value of two floating point arguments `x` and `y.`

See also: [`fl.min`](#link666c2e6d696e), [`max`](#link6d6178), [`min`](#link6d696e).	 [→index](#idx)

### `fl.mod` : procedure/2

Usage: `(fl.mod x y) => fl`

Return the floating point remainder of `x` / `y.`

See also: [`fl.remainder`](#link666c2e72656d61696e646572).	 [→index](#idx)

### `fl.modf` : procedure/1

Usage: `(fl.modf x) => li`

Return  integer and fractional floating-point numbers that sum to `x`. Both values have the same sign as `x.`

See also: [`fl.mod`](#link666c2e6d6f64).	 [→index](#idx)

### `fl.nan` : procedure/1

Usage: `(fl.nan) => fl`

Return the IEEE 754 not-a-number value.

See also: [`fl.is-nan?`](#link666c2e69732d6e616e3f), [`fl.inf`](#link666c2e696e66).	 [→index](#idx)

### `fl.next-after` : procedure/1

Usage: `(fl.next-after x) => fl`

Return the next representable floating point number after `x.`

See also: [`fl.is-nan?`](#link666c2e69732d6e616e3f), [`fl.nan`](#link666c2e6e616e), [`fl.inf`](#link666c2e696e66).	 [→index](#idx)

### `fl.pow` : procedure/2

Usage: `(fl.pow x y) => fl`

Return `x` to the power of `y` according to 64 bit floating point arithmetics.

See also: [`fl.pow10`](#link666c2e706f773130).	 [→index](#idx)

### `fl.pow10` : procedure/1

Usage: `(fl.pow10 n) => fl`

Return 10 to the power of integer `n` as a 64 bit floating point number.

See also: [`fl.pow`](#link666c2e706f77).	 [→index](#idx)

### `fl.remainder` : procedure/2

Usage: `(fl.remainder x y) => fl`

Return the IEEE 754 floating-point remainder of `x` / `y.`

See also: [`fl.mod`](#link666c2e6d6f64).	 [→index](#idx)

### `fl.round` : procedure/1

Usage: `(fl.round x) => fl`

Round `x` to the nearest integer floating point number according to floating point arithmetics.

See also: [`fl.round-to-even`](#link666c2e726f756e642d746f2d6576656e), [`fl.truncate`](#link666c2e7472756e63617465), [`int`](#link696e74), [`float`](#link666c6f6174).	 [→index](#idx)

### `fl.round-to-even` : procedure/1

Usage: `(fl.round-to-even x) => fl`

Round `x` to the nearest even integer floating point number according to floating point arithmetics.

See also: [`fl.round`](#link666c2e726f756e64), [`fl.truncate`](#link666c2e7472756e63617465), [`int`](#link696e74), [`float`](#link666c6f6174).	 [→index](#idx)

### `fl.signbit` : procedure/1

Usage: `(fl.signbit x) => bool`

Return true if `x` is negative, nil otherwise.

See also: [`fl.abs`](#link666c2e616273).	 [→index](#idx)

### `fl.sin` : procedure/1

Usage: `(fl.sin x) => fl`

Return the sine of `x.`

See also: [`fl.cos`](#link666c2e636f73).	 [→index](#idx)

### `fl.sinh` : procedure/1

Usage: `(fl.sinh x) => fl`

Return the hyperbolic sine of `x.`

See also: [`fl.sin`](#link666c2e73696e).	 [→index](#idx)

### `fl.sqrt` : procedure/1

Usage: `(fl.sqrt x) => fl`

Return the square root of `x.`

See also: [`fl.pow`](#link666c2e706f77).	 [→index](#idx)

### `fl.tan` : procedure/1

Usage: `(fl.tan x) => fl`

Return the tangent of `x` in radian.

See also: [`fl.tanh`](#link666c2e74616e68), [`fl.sin`](#link666c2e73696e), [`fl.cos`](#link666c2e636f73).	 [→index](#idx)

### `fl.tanh` : procedure/1

Usage: `(fl.tanh x) => fl`

Return the hyperbolic tangent of `x.`

See also: [`fl.tan`](#link666c2e74616e), [`flsinh`](#link666c73696e68), [`fl.cosh`](#link666c2e636f7368).	 [→index](#idx)

### `fl.trunc` : procedure/1

Usage: `(fl.trunc x) => fl`

Return the integer value of `x` as floating point number.

See also: [`truncate`](#link7472756e63617465), [`int`](#link696e74), [`fl.floor`](#link666c2e666c6f6f72).	 [→index](#idx)

### `fl.y0` : procedure/1

Usage: `(fl.y0 x) => fl`

Return the order-zero Bessel function of the second kind applied to `x.`

See also: [`fl.y1`](#link666c2e7931), [`fl.yn`](#link666c2e796e), [`fl.j0`](#link666c2e6a30), [`fl.j1`](#link666c2e6a31), [`fl.jn`](#link666c2e6a6e).	 [→index](#idx)

### `fl.y1` : procedure/1

Usage: `(fl.y1 x) => fl`

Return the order-one Bessel function of the second kind applied to `x.`

See also: [`fl.y0`](#link666c2e7930), [`fl.yn`](#link666c2e796e), [`fl.j0`](#link666c2e6a30), [`fl.j1`](#link666c2e6a31), [`fl.jn`](#link666c2e6a6e).	 [→index](#idx)

### `fl.yn` : procedure/1

Usage: `(fl.yn n x) => fl`

Return the Bessel function of the second kind of order `n` applied to `x`. Argument `n` must be an integer value.

See also: [`fl.y0`](#link666c2e7930), [`fl.y1`](#link666c2e7931), [`fl.j0`](#link666c2e6a30), [`fl.j1`](#link666c2e6a31), [`fl.jn`](#link666c2e6a6e).	 [→index](#idx)





## Graphical User Interface {#gui}

This section describes the GUI-related functions. These functions are only available when Z3S5 Lisp has been compiled with the embedded GUI package. See the `z3g` executable defined in `cmd/z3g/z3g.go` for an example of how to include the GUI and start it up. The key is that the interpreter must run in its own goroutine because the GUI is blocking once it has been called. GUI functions must be called with one of the GUI-embedded functions `gui`, `gui+`, and `gui*` at least once, as well as every time when they are called from a future or task in order to ensure they are called in main thread of the operating system. This is a requirement of the `Fyne` framework on which this GUI framework is based. (The Fyne equivalent to `gui` is `fyne.Do` and the equivalent to `gui*` is `fyne.DoAndWait`.) Fyne's functions are mapped to Z3S5 Lisp and listed under the 'gui label in this help system. The naming conventions for translation between Go and Lisp functions are as follows:

1. Camelcase is translated to lowercase with hyphens.
2. A function `object.VerbQualifier` becomes verb-object-qualifier.
3. Getters are written in the form `get-object-qualifier` and setters `set-object-qualifier`.
4. As an exception of the previous rules, when the result of a function is a bool, the form is `object-predicate?`.

Fyne objects are represented by integer numbers. The system internally translates between these numbers and objects. Occasionally, Fyne objects are created on the fly for performance reasons. For example, sometimes color lists of the form `(r g b a)` with integers `r`, `g`,`b`, `a` are used instead of creating and storing color objects using `(nrgba r g b a)`. There are also sometimes shortcut accessors using selector symbols and other convenience wrappers for Fyne functions. When in doubt, refer to the Lisp help for details.

When importing the GUI with `DefGUI`, a `Config` structure is provided that allows for restricted security. This makes it possible to use the GUI functions in a restricted environment that e.g. does not allow the creation of new windows.

### `activate-menu-last-submenu` : procedure/1

Usage: `(activate-menu-last-submenu menu) => bool`

Find the last active menu item traversing through open submenus, and activate its submenu if one is found. Return true if a submenu was activated, nil otherwise.

See also: [`deactivate-menu-last-submenu`](#link646561637469766174652d6d656e752d6c6173742d7375626d656e75), [`new-menu`](#link6e65772d6d656e75), [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874), [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573).	 [→index](#idx)

### `activate-menu-next` : procedure/1

Usage: `(activate-menu-next menu)`

Activate the menu item following the currently active menu item, if there is any.

See also: [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573), [`new-menu`](#link6e65772d6d656e75), [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75).	 [→index](#idx)

### `activate-menu-previous` : procedure/1

Usage: `(activate-menu-previous menu)`

Activate the menu item before the currently active menu item, if there is any.

See also: [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874), [`new-menu`](#link6e65772d6d656e75), [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75).	 [→index](#idx)

### `add-canvas-shortcut` : procedure/3

Usage: `(add-canvas-shortcut canvas shortcut proc)`

Add the given `shortcut` to the given `canvas`, calling the handler `proc` when it is triggered. `shortcut` must be a list consisting of valid keyboard modifier symbols and a valid key symbol. `proc` must be a function that takes a shortcut as argument. If multiple non-modifier keys are present, only the last one is taken. However, multiple modifier keys are possible. Possible modifiers are symbols or corresponding strings in '(shift control alt suprt). Possible keys are in '(escape return tab backspace insert delete right left down up page-up page-down home end f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 enter 0 1 2 3 4 5 6 7 8 9 key0 key1 key2 key3 key4 key5 key6 key7 key8 key9 a b c d e f g h i j k l m n o p q r s t u v w x y z space tick comma minus period slash backslash left-bracket right-bracket semicolon equal asterisk plus back-tick) and their string variants.

See also: [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`remove-canvas-shortcut`](#link72656d6f76652d63616e7661732d73686f7274637574).	 [→index](#idx)

### `add-lisp-editor` : procedure/1

Usage: `(add-lisp-editor win)`

Add a lisp editor to the given window. This includes automatic help and looking up functions.

See also: [`lisped`](#link6c6973706564).	 [→index](#idx)

### `append-form` : procedure/0

Usage: `(append-form form str canvas-object)`

Append a new row to the bottom `form` consisting of a label `str` aligned with a `canvas-object`, which may be an entry, button, etc.

See also: [`new-form`](#link6e65772d666f726d).	 [→index](#idx)

### `center-window-on-screen` : procedure/1

Usage: `(center-window-on-screen window)`

As the name implies, this function centers the window on the screen.

See also: [`set-window-full-screen`](#link7365742d77696e646f772d66756c6c2d73637265656e), [`window-full-screen?`](#link77696e646f772d66756c6c2d73637265656e3f).	 [→index](#idx)

### `close-gui` : procedure/0

Usage: `(close-gui)`

Close the GUI, freeing all resources associated with it. After this function has been called, no further GUI functions can be used.

See also: [`forget-gui-object`](#link666f726765742d6775692d6f626a656374), [`close-window`](#link636c6f73652d77696e646f77).	 [→index](#idx)

### `close-window` : procedure/1

Usage: `(close-window window)`

Closes `window` and delete it from internal storage. This frees window resources. It cannot be re-used after this operation. Use window-hide if you want to close/hide a window only temporarily. Notice that unlike in Fyne, there is no mechanism to close an application automatically after its main window has been closed.

See also: [`hide-window`](#link686964652d77696e646f77).	 [→index](#idx)

### `color->color64` : procedure/1

Usage: `(color->color64 li) => li`

Convert a 32-bit NRGBA color list with values from 0 to 255 (inclusive) as it is used by Z3S5 Lisp's *color* dict to a 64-bit NRGBA64 color list with values from 0 to 65635 (inclusive) as they are used by the GUI.

See also: [`theme-color`](#link7468656d652d636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a).	 [→index](#idx)

### `color64->color` : procedure/1

Usage: `(color64->color li) => li`

Convert a 64-bit NRGBA64 color list with values from 0 to 65365 (inclusive) to a 32-bit color list with values from 0 to 255 (inclusive) as they are used by Z3S5 Lisp's *colors* dict.

See also: [`theme-color`](#link7468656d652d636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a).	 [→index](#idx)

### `count-text-grid-row-columns` : procedure/2

Usage: `(count-text-grid-row-columns grid row) => int`

Return the number of columns in `row` of `grid`, 0 if there are none.

See also: [`count-text-grid-rows`](#link636f756e742d746578742d677269642d726f7773), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77).	 [→index](#idx)

### `count-text-grid-rows` : procedure/1

Usage: `(count-text-grid-rows grid) => int`

Return the number of the last row in grid, 0 if there are none.

See also: [`count-text-grid-row-columns`](#link636f756e742d746578742d677269642d726f772d636f6c756d6e73), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77).	 [→index](#idx)

### `create-lorem-ipsum` : procedure/3

Usage: `(create-lorem-ipsum selector min max) => str`

Create random Lorem Ipsum fill text based on `selector`. If `selector` is 'word, then a word with at least `min` letters and at most `max` letters is created. If `selector` is 'sentence, then a sentence with at least `min` words and at most `max` words is created. If `selector` is 'paragraph, then a paragraph with at least `min` sentences and at most `max` sentences is created.

See also: [`new-zedit`](#link6e65772d7a65646974), [`new-text-grid`](#link6e65772d746578742d67726964), [`new-entry`](#link6e65772d656e747279).	 [→index](#idx)

### `deactivate-menu-child` : procedure/1

Usage: `(deactivate-menu-child menu)`

Deactivate the currently active menu item and close its submenu if there is one.

See also: [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75), [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874), [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573), [`new-menu`](#link6e65772d6d656e75).	 [→index](#idx)

### `deactivate-menu-last-submenu` : procedure/1

Usage: `(deactivate-menu-last-submenu menu)`

Traverse the menu and deactivate the last open submenu found.

See also: [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75), [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874), [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573), [`new-menu`](#link6e65772d6d656e75).	 [→index](#idx)

### `delete-zedit-all` : procedure/1

Usage: `(delete-zedit-all editor)`

Delete all text in the editor and corresponding tags. Tag stylers are preserved.

See also: [`delete-zedit`](#link64656c6574652d7a65646974).	 [→index](#idx)

### `disable-object` : procedure/1

Usage: `(disable-object obj)`

Disable the canvas object `obj.`

See also: [`enable-object`](#link656e61626c652d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `enable-object` : procedure/1

Usage: `(enable-object obj)`

Enable the canvas object `obj.`

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `entry-accepts-tab?` : procedure/1

Usage: `(entry-accepts-tab? entry) => bool`

Return `nil` when the entry does not accept tabs, `t` otherwise.

See also: [`new-entry`](#link6e65772d656e747279).	 [→index](#idx)

### `focus-canvas-object` : procedure/2

Usage: `(focus-canvas-object canvas object)`

Set the focus within `canvas` to `object`. The object must be a focusable canvas object such as an entry or button.

See also: [`focus-zedit`](#link666f6375732d7a65646974), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`get-focused-canvas-object`](#link6765742d666f63757365642d63616e7661732d6f626a656374), [`focus-next-canvas-object`](#link666f6375732d6e6578742d63616e7661732d6f626a656374), [`focus-previous-canvas-object`](#link666f6375732d70726576696f75732d63616e7661732d6f626a656374), [`unfocus-canvas-objects`](#link756e666f6375732d63616e7661732d6f626a65637473).	 [→index](#idx)

### `focus-next-canvas-object` : procedure/1

Usage: `(focus-next-canvas-object canvas)`

Focus the next focusable user interface element in `canvas.`

See also: [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374), [`focus-previous-canvas-object`](#link666f6375732d70726576696f75732d63616e7661732d6f626a656374), [`unfocus-canvas-objects`](#link756e666f6375732d63616e7661732d6f626a65637473), [`get-focused-canvas-object`](#link6765742d666f63757365642d63616e7661732d6f626a656374).	 [→index](#idx)

### `focus-previous-canvas-object` : procedure/1

Usage: `(focus-previous-canvas-object canvas)`

Focus the previous focusable user interface element in `canvas.`

See also: [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374), [`focus-next-canvas-object`](#link666f6375732d6e6578742d63616e7661732d6f626a656374), [`unfocus-canvas-objects`](#link756e666f6375732d63616e7661732d6f626a65637473), [`get-focused-canvas-object`](#link6765742d666f63757365642d63616e7661732d6f626a656374).	 [→index](#idx)

### focus-zedit : nil

Usage: `(focus-zedit editor)`

Set the focus to the given `editor`. This needs to be used instead of `focus-canvas-object` because a zedit is internally a complex widget and not a canvas object.

See also: [`new-zedit`](#link6e65772d7a65646974), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374).	 [→index](#idx)

### `forget-gui-object` : procedure/1

Usage: `(forget-gui-object int)`

Forget the GUI object `int`. This removes any association with the object but does not free internal resources if the object still exists. Internal use only.

See also: [`close-window`](#link636c6f73652d77696e646f77), [`close-gui`](#link636c6f73652d677569).	 [→index](#idx)

### `get-clipboard-content` : procedure/0

Usage: `(get-clipboard-content) => str`

Return the current content of the operating system clipboard as string. This function might raise an error if clipboard access is prohibited by host security settings.

See also: [`set-clipboard-content`](#link7365742d636c6970626f6172642d636f6e74656e74).	 [→index](#idx)

### `get-device-info` : procedure/0

Usage: `(get-device-info) => li`

Return a list with information about the current host device. This returns an association list where 'orientation might be one of '(vertical vertical-upside-down left right unknown), self-explanatory boolean keys 'is-mobile?, 'is-browser, 'has-keyboard?, and 'system-scale with the current scaling factor for graphics as float. The system scale is used to dynamically scale user interface elements to remain legible on hi res displays.

See also: [`close-gui`](#link636c6f73652d677569).	 [→index](#idx)

### `get-entry-cursor` : procedure/1

Usage: `(get-entry-cursor entry) => sym`

Return a symbol that represents the current cursor of `entry`. Possible values are in '(default text crosshair pointer hresize vresize). Curiously, there is no way to set the cursor yet.

See also: [`new-entry`](#link6e65772d656e747279).	 [→index](#idx)

### `get-entry-cursor-pos` : procedure/1

Usage: `(get-entry-cursor-pos entry) => li`

Return a list consisting of row number and column number of the current cursor position of the cursor in `entry.`

See also: [`set-entry-cursor-row`](#link7365742d656e7472792d637572736f722d726f77), [`set-entry-cursor-column`](#link7365742d656e7472792d637572736f722d636f6c756d6e).	 [→index](#idx)

### `get-focused-canvas-object` : procedure/1

Usage: `(get-focused-canvas-object canvas) => int`

Obtain the canvas object that is currently focused in `canvas`, or nil if there is none.

See also: [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374), [`focus-next-canvas-object`](#link666f6375732d6e6578742d63616e7661732d6f626a656374), [`focus-previous-canvas-object`](#link666f6375732d70726576696f75732d63616e7661732d6f626a656374).	 [→index](#idx)

### `get-label-text` : procedure/1

Usage: `(get-label-text label) => str`

Gets the text of `label`

See also: [`set-label-text`](#link7365742d6c6162656c2d74657874), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `get-menu-item-label` : procedure/1

Usage: `(get-menu-item-label item) => str`

Return the current label of the given menu `item.`

See also: [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`menu-item-disabled`](#link6d656e752d6974656d2d64697361626c6564), [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx)

### `get-object-min-size` : procedure/1

Usage: `(get-object-min-size obj) => li`

Return the minimum size of canvas object `obj` as a list containing the width and height as floats. The minimum size is computed based on various internal criteria and can only be changed for some special widgets.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `get-object-position` : procedure/1

Usage: `(get-object-position obj) => li`

Return the position of canvas object `obj` as a list containing the x and y coordinates as floats.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `get-object-size` : procedure/1

Usage: `(get-object-size obj) => li`

Return the size of canvas object `obj` as a list containing the width and height as floats.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `get-progress-bar-value` : procedure/1

Usage: `(get-progress-bar-value bar) => num`

Return the current value of progress-bar `bar.`

See also: [`set-progress-bar`](#link7365742d70726f67726573732d626172), [`new-progress-bar`](#link6e65772d70726f67726573732d626172), [`new-infinite-progress-bar`](#link6e65772d696e66696e6974652d70726f67726573732d626172).	 [→index](#idx)

### `get-scroll-offset` : procedure/1

Usage: `(get-scroll-offset scroll) => li`

Get the offset of `scroll`, which may be a hscroll, vscroll, or scroll, as a position list of (x y) where `x` and `y` are floats.

See also: [`set-scroll-offset`](#link7365742d7363726f6c6c2d6f6666736574), [`new-scroll`](#link6e65772d7363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c), [`new-vscroll`](#link6e65772d767363726f6c6c).	 [→index](#idx)

### `get-text-grid-cell` : procedure/3

Usage: `(get-text-grid-cell grid row column) => li`

Return the cell of `grid` at `row` and `column`. The result is a list consisting of a string containing one unicode rune and a grid style list. The style might be nil. If it is not nil, then the list contains a foreground and a background color list.

See also: [`get-text-grid-rune`](#link6765742d746578742d677269642d72756e65), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`get-text-grid-style`](#link6765742d746578742d677269642d7374796c65).	 [→index](#idx)

### `get-text-grid-cell-size` : procedure/1

Usage: `(get-text-grid-cell-size grid) => li`

Return the size of one text grid cell as a list of floats (w h) where `w` is the width and `h` is the height.

See also: [`new-text-grid`](#link6e65772d746578742d67726964).	 [→index](#idx)

### `get-text-grid-row` : procedure/2

Usage: `(get-text-grid-row grid row) => li`

Obtain a `row` of a text `grid`, where `row` is a 0-based index. This function returns a list of the form '(row style), where `style` is a grid style list and `row` is an array of lists consisting each of a unicode string containing one rune and a grid style list. Each entry of the `row` array represents an individual unicode glyph with a style, whereas the `style` list in the return argument represents an optional style of the whole row.

See also: [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874), [`remove-text-grid-row`](#link72656d6f76652d746578742d677269642d726f77), [`insert-text-grid-row`](#link696e736572742d746578742d677269642d726f77).	 [→index](#idx)

### `get-text-grid-row-text` : procedure/2

Usage: `(get-text-grid-row-text grid row) => str`

Return the text of `row` in `grid` as a string without any style information.

See also: [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx)

### `get-text-grid-rune` : procedure/3

Usage: `(get-text-grid-rune grid row column) => str`

Return the string containing a single rune at the cell in `row` and `column` of `grid.`

See also: [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`get-text-grid-style`](#link6765742d746578742d677269642d7374796c65), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77).	 [→index](#idx)

### `get-text-grid-tab-width` : procedure/1

Usage: `(get-text-grid-tab-width grid) => int`

Return the current tabulator width of `grid` in space characters.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx)

### `get-text-grid-text` : procedure/1

Usage: `(get-text-grid-text grid) => str`

Return the text of `grid` as a string without style information.

See also: [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`new-text-grid`](#link6e65772d746578742d67726964), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-rune`](#link6765742d746578742d677269642d72756e65), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c).	 [→index](#idx)

### `get-window-canvas` : procedure/1

Usage: `(get-window-canvas window) => int`

Get the canvas object of `window`, which is the area on which window elements are drawn. This is not the same as the window-content, which is a widget or other user interface element. The canvas is used for raw-drawing commands, for example for drawing circles and boxes. With a suitable layout that doesn't re-arrange objects, it can e.g. be used to draw overlays.

See also: [`get-window-content`](#link6765742d77696e646f772d636f6e74656e74), [`set-window-content`](#link7365742d77696e646f772d636f6e74656e74), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374).	 [→index](#idx)

### `get-window-content` : procedure/2

Usage: `(get-window-content window) => int`

Get the canvas object ID that represents the main content of the window. This is usually a widget or a container with layout.

See also: [`set-window-content`](#link7365742d77696e646f772d636f6e74656e74), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173).	 [→index](#idx)

### `get-window-icon` : procedure/1

Usage: `(get-window-icon window) => int`

Obtain the icon ID of the icon of `window`. The resource obtained is not guaranteed to be a visible icon or might be a dummy, as not all windows have icons on all platforms.

See also: [`set-window-icon`](#link7365742d77696e646f772d69636f6e), [`new-icon`](#link6e65772d69636f6e), [`theme-icon`](#link7468656d652d69636f6e).	 [→index](#idx)

### `get-window-main-menu` : procedure/1

Usage: `(get-window-main-menu window) => int`

Get the main menu ID of `window.`

See also: [`set-window-main-menu`](#link7365742d77696e646f772d6d61696e2d6d656e75), [`new-main-menu`](#link6e65772d6d61696e2d6d656e75).	 [→index](#idx)

### `get-window-title` : procedure/1

Usage: `(get-window-title window) => str`

Return the title of `window` as string.

See also: [`set-window-title`](#link7365742d77696e646f772d7469746c65).	 [→index](#idx)

### `get-zedit-char-at` : procedure/2

Usage: `(get-zedit-char-at zedit li) => str`

Return the character (unicode glyph) at position `li` in `zedit` as a string. The position `li` may contain a line, a line and column, or a line, column, and bool (the latter is ignored).

See also: [`get-zedit-text`](#link6765742d7a656469742d74657874), [`get-zedit-text-range`](#link6765742d7a656469742d746578742d72616e6765), [`get-zedit-next-pos`](#link6765742d7a656469742d6e6578742d706f73), [`get-zedit-prev-pos`](#link6765742d7a656469742d707265762d706f73), [`new-zedit`](#link6e65772d7a65646974), [`insert-zedit`](#link696e736572742d7a65646974), [`delete-zedit`](#link64656c6574652d7a65646974).	 [→index](#idx)

### `get-zedit-columns` : procedure/1

Usage: `(get-zedit-columns editor) => int`

Get the number of columns in the editor. These are the columns that are displayed. If word wrapping is off, lines may have more columns.

See also: [`get-zedit-lines`](#link6765742d7a656469742d6c696e6573).	 [→index](#idx)

### `get-zedit-config` : procedure/2

Usage: `(get-zedit-config zedit sel) => any`

Set a configuration property of the given `zedit` to `b`. The selector `sel` can be one of the following: show-line-numbers? - show the line number display if true, show-whitespace? - show whitespace characters if true, line-wrap? - wrap lines to the size of the widget automatically, soft-wrap? - do not end lines in a hard line feed when line wrapping (default), draw-caret? - draw the caret if true, supress caret drawing if false, highlight-parens? - automatically highlight the matching opening paren while typing and moving the caret after a closing paren, highlight-paren-range? - automatically highlight the range of characters between the opening and closing paren when hightlight-parens? is true, get-word-at-left? - returns word left of caret if true, otherwise words are only triggered when the caret is on them, liberal-get-word-at? - words are parsed more like Z3S5 Lisp symbols, i.e., including all kinds of delimiters except for parentheses.

See also: [`new-zedit`](#link6e65772d7a65646974), [`set-zedit-config`](#link7365742d7a656469742d636f6e666967).	 [→index](#idx)

### `get-zedit-last-column` : procedure/2

Usage: `(get-zedit-last-column zedit line) => num`

Get the 0-based column number of the last column of the given `line` in editor `zedit.`

See also: [`new-zedit`](#link6e65772d7a65646974), [`get-zedit-last-line`](#link6765742d7a656469742d6c6173742d6c696e65).	 [→index](#idx)

### `get-zedit-lines` : procedure/1

Usage: `(get-zedit-lines editor) => int`

Get the number of lines in the editor.

See also: [`get-zedit-columns`](#link6765742d7a656469742d636f6c756d6e73).	 [→index](#idx)

### `get-zedit-next-pos` : procedure/2

Usage: `(get-zedit-next-pos zedit pos) => li`

Get the next position after `pos` in editor `zedit`. The returned list consists of a line, column, and a bool. The bool is true when the position is new and nil when `pos` is already the last position, in case of which the position returned is also the same as `pos.`

See also: [`get-zedit-prev-pos`](#link6765742d7a656469742d707265762d706f73), [`get-zedit-char-at`](#link6765742d7a656469742d636861722d6174).	 [→index](#idx)

### `get-zedit-prev-pos` : procedure/2

Usage: `(get-zedit-prev-pos zedit pos) => li`

Get the previous position after `pos` in editor `zedit`. The returned list consists of a line, column, and a bool. The bool is true when the position is new and nil when position (0 0) has been reached, in case of which the position returned is (0 0 nil).

See also: [`get-zedit-next-pos`](#link6765742d7a656469742d6e6578742d706f73), [`get-zedit-char-at`](#link6765742d7a656469742d636861722d6174).	 [→index](#idx)

### `get-zedit-text` : procedure/1

Usage: `(get-zedit-text zedit) => str`

Get the text in editor `zedit.`

See also: [`new-zedit`](#link6e65772d7a65646974), [`get-zedit-text`](#link6765742d7a656469742d74657874), [`get-zedit-text-range`](#link6765742d7a656469742d746578742d72616e6765), [`zedit-current-selection-text`](#link7a656469742d63757272656e742d73656c656374696f6e2d74657874).	 [→index](#idx)

### `get-zedit-text-range` : procedure/2

Usage: `(get-zedit-text-range zedit interval) => str`

Obtain the text in `zedit` within range `interval` consisting of a list of two lists for the start and end position each. The function returns an empty string if there is no text at the interval.

See also: [`get-zedit-text`](#link6765742d7a656469742d74657874), [`zedit-current-selection-text`](#link7a656469742d63757272656e742d73656c656374696f6e2d74657874).	 [→index](#idx)

### `get-zedit-top-line` : procedure/1

Usage: `(get-zedit-top-line zedit) => int`

Get the currently topmost visible line in the editor `zedit.`

See also: [`set-zedit-top-line`](#link7365742d7a656469742d746f702d6c696e65).	 [→index](#idx)

### `gui` : macro/0 or more

Usage: `(gui body...)`

The `gui` macro ensures that all GUI function calls are executed in the OS main thread. Since the interpreter starts in its own thread, methods have to be embedded into `gui`, `gui+`, or gui* when they are called. Several GUI calls and other Lisp expressions can be put into the body of `gui` and will be called sequentially like in `progn`. The `gui` macros also need to be used whenever a GUI method is called from a task or future. See the GUI demo for examples of how to use them. When a GUI function is not embedded in one of the `gui` macros, a warning message is printed.

See also: [`gui+`](#link6775692b), [`gui*`](#link6775692a).	 [→index](#idx)

### `gui*` : macro/0 or more

Usage: `(gui* body...) => any`

Like `gui`, but this macro blocks execution until all expressions in `body` have been evaluated in the main OS thread and returns the result of evaluating the last expression like `progn`. All GUI functions must be called with one of the `gui` macros, see the help entry for `gui` for more information.

See also: [`gui`](#link677569), [`gui*`](#link6775692a).	 [→index](#idx)

### `gui+` : macro/0 or more

Usage: `(gui+ body...) => future`

Like `gui`, this macro ensures that all functions in the body are called in the OS main thread. All GUI functions must be called with one of the `gui` macros, see the help entry for `gui` for more information. `gui+` returns a future that represents the result of evaluating the last `body` expression like in `progn` but as a future.

See also: [`gui`](#link677569), [`gui*`](#link6775692a).	 [→index](#idx)

### `hide-object` : procedure/1

Usage: `(hide-object obj)`

Hide the canvas object `obj.`

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `hide-window` : procedure/1

Usage: `(hide-window window)`

Hides `window`. It can be shown again using show-window.

See also: [`show-window`](#link73686f772d77696e646f77), [`close-window`](#link636c6f73652d77696e646f77).	 [→index](#idx)

### `insert-text-grid-row` : procedure/2

Usage: `(insert-text-grid-row grid row)`

Insert a new text grid row before `row` in the given text `grid`. If `row` is the number of rows, a new row is appended to the end of the text grid.

See also: [`remove-text-grid-row`](#link72656d6f76652d746578742d677269642d726f77), [`count-text-grid-rows`](#link636f756e742d746578742d677269642d726f7773), [`new-text-grid`](#link6e65772d746578742d67726964), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77).	 [→index](#idx)

### `list-uri` : procedure/1

Usage: `(list-uri uri) => vec`

List a listable URI string `uri` if possible. If the URI string is not valid or valid but not listable, an error occurs. A listable URI is a file URI pointing to a folder, for example.

See also: [`show-folder-open`](#link73686f772d666f6c6465722d6f70656e), [`listable-uri?`](#link6c69737461626c652d7572693f), [`uri?`](#link7572693f).	 [→index](#idx)

### `listable-uri?` : procedure/1

Usage: `(listable-uri? s) => bool`

Return true if the string `s` represents a listable URI such as one pointing to a folder on the filesystem; otherwise, nil is returned.

See also: [`uri?`](#link7572693f), [`list-uri`](#link6c6973742d757269).	 [→index](#idx)

### `make-or-get-zedit-style-tag` : procedure/4

Usage: `(make-or-get-zedit-style-tag editor style draw-full-line?) => int`

Create a new tag or get an existing tag for marking the style of text. The style must be an a-list containing with boolean keys in '(bold italic monospace) followed by nil or non-nil, or keys 'text-color or 'background-color followed by a color list with 16-bit r g b a values. If `draw-full-line?` is not nil, then the full line is drawn (like in a selection) when multiple lines are drawn; otherwise, only actual text is drawn with the color and not whitespace at the end of a line. The function returns a tag ID and automatically adds the corresponding style function to render the text in the given color.

See also: [`new-zedit`](#link6e65772d7a65646974), [`color64->color`](#link636f6c6f7236342d3e636f6c6f72), [`color->color64`](#link636f6c6f722d3e636f6c6f723634), [`theme-color`](#link7468656d652d636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a).	 [→index](#idx)

### `menu-item-checked?` : procedure/1

Usage: `(menu-item-checked? item) => bool`

Return true if `item` is currently checked, nil otherwise.

See also: [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`menu-item-disabled?`](#link6d656e752d6974656d2d64697361626c65643f), [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx)

### `menu-item-disabled?` : procedure/1

Usage: `(menu-item-disabled? item) => bool`

Return true if `item` is currently disabled, nil otherwise.

See also: [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx)

### `move-object` : procedure/2

Usage: `(move-object obj position)`

Move the canvas object `obj` to the given `position` list, containing its `x` and `y` coordinates as floats.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `new-app-tabs` : procedure/0 or more

Usage: `(new-app-tabs tab-item ...) => int`

Create a new application tabs, which allow users to choose different items within an application.

See also: [`new-doc-tabs`](#link6e65772d646f632d74616273), [`new-tabitem`](#link6e65772d7461626974656d), [`new-tabitem-with-icon`](#link6e65772d7461626974656d2d776974682d69636f6e).	 [→index](#idx)

### `new-border` : procedure/4 or more

Usage: `(new-border top bottom left right [obj ...]) => int`

Create a new border layout, which is one of the most useful layouts. Any of `top`, `bottom`, `left`, and `right` is put in the respective place (with minimum size) and might also be `nil` for no widget. The remaining canvas objects `obj` are arranged in the center and take maximum size. This allows you e.g. to put a list on the left side of a window, a panel of buttons on the top, and the main content in another container in the center.

See also: [`new-container`](#link6e65772d636f6e7461696e6572), [`new-container-without-layout`](#link6e65772d636f6e7461696e65722d776974686f75742d6c61796f7574), [`new-vscroll`](#link6e65772d767363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c).	 [→index](#idx)

### `new-button` : procedure/2

Usage: `(new-button label proc) => int`

Return a new button with the given `label` and without an icon. The callback `proc` is called without arguments when the button is pressed or tapped.

See also: [`new-button-with-icon`](#link6e65772d627574746f6e2d776974682d69636f6e), [`new-hyperlink`](#link6e65772d68797065726c696e6b), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `new-button-with-icon` : procedure/3

Usage: `(new-button-with-icon label icon proc) => int`

Return a new button the given `label` and `icon`. The callback `proc` is called without arguments when the button is pressed.

See also: [`new-button`](#link6e65772d627574746f6e), [`new-icon`](#link6e65772d69636f6e), [`theme-icon`](#link7468656d652d69636f6e).	 [→index](#idx)

### `new-center-layout` : procedure/0

Usage: `(new-center-layout) => int`

Create a new center layout, which centers container elements (possibly overlapping). This may be used for drawing centered on the window, for example.

See also: [`new-form`](#link6e65772d666f726d), [`append-form`](#link617070656e642d666f726d), [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx)

### `new-check` : procedure/2

Usage: `(new-check title proc) => int`

Create and return a new check box with the given `title` string and a callback procedure `proc`. The callback `proc` is called with the new state of the check box as bool when it has changed.

See also: [`new-choice`](#link6e65772d63686f696365).	 [→index](#idx)

### `new-choice` : procedure/3

Usage: `(new-choice selector string-list proc) => int`

Create and return a new choice representing choices in `string-list`. If `selector` is 'radio-group, a group of radio buttons is created with options in `string-list`. If `selector` is 'select, a more compact selection menu is created with the options in `string-list`. The callback `proc` takes a string that represents the choice that has been selected.

See also: [`set-select-options`](#link7365742d73656c6563742d6f7074696f6e73), [`new-check`](#link6e65772d636865636b).	 [→index](#idx)

### `new-circle` : procedure/1 or more

Usage: `(new-circle fill-color [pos1] [pos2] [stroke-color] [stroke-width]) => int`

Draw and return a circle with the given NRGBA `fill-color`. If the optional `pos1` and `pos2` position lists of `x` and `y` coordinates in floats are given , then the circle is drawn inside the rectangle defined by these positions. The optional `stroke-color` and `stroke-width` arguments determine the outline of the circle. Notice that circle's size and position may be set by the layout of the container, so to set these manually using `pos1` and `pos2` you need to make sure the underlying container has no such layout.

See also: [`new-rectangle`](#link6e65772d72656374616e676c65), [`new-line-new-text`](#link6e65772d6c696e652d6e65772d74657874).	 [→index](#idx)

### `new-combined-string-validator` : procedure/1 or more

Usage: `(new-combined-string-validator validator-1 [...validator-n]) => int`

Combine validators `validator-1` to `validator-n` into a combined string validator and return it.

See also: [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72), [`new-validator`](#link6e65772d76616c696461746f72), [`new-regexp-validator`](#link6e65772d7265676578702d76616c696461746f72), [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72), [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b), [`validate-object`](#link76616c69646174652d6f626a656374).	 [→index](#idx)

### `new-container` : procedure/1 or more

Usage: `(new-container layout obj ...) => int`

Create a new container with the given `layout` and various canvas objects `obj` arranged by the layout.

See also: [`new-container-without-layout`](#link6e65772d636f6e7461696e65722d776974686f75742d6c61796f7574), [`new-border`](#link6e65772d626f72646572), [`new-vscroll`](#link6e65772d767363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c).	 [→index](#idx)

### `new-container-without-layout` : procedure/0 or more

Usage: `(new-container-without-layout obj ...) => int`

Create a new container without a layout (overlapping objects) with the given canvas objects `obj.`

See also: [`new-container`](#link6e65772d636f6e7461696e6572), [`new-border`](#link6e65772d626f72646572).	 [→index](#idx)

### `new-doc-tabs` : procedure/0 or more

Usage: `(new-doc-tabs tab-item ...) => int`

Create new document tabs, which allow users to choose different items in a window (not the application as a whole like app-tabs).

See also: [`new-app-tabs`](#link6e65772d6170702d74616273), [`new-tabitem`](#link6e65772d7461626974656d), [`new-tabitem-with-icon`](#link6e65772d7461626974656d2d776974682d69636f6e).	 [→index](#idx)

### `new-entry` : procedure/1

Usage: `(new-entry [selector]) => int`

Create a new text entry field based on the optional `selector` symbol. `selector` can be a symbol in '(single-line multi-line password). The default is 'single-line.

See also: [`set-entry-on-change-callback`](#link7365742d656e7472792d6f6e2d6368616e67652d63616c6c6261636b), [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72), [`entry-accepts-tab?`](#link656e7472792d616363657074732d7461623f), [`get-entry-cursor-pos`](#link6765742d656e7472792d637572736f722d706f73), [`set-entry-cursor-row`](#link7365742d656e7472792d637572736f722d726f77), [`set-entry-cursor-column`](#link7365742d656e7472792d637572736f722d636f6c756d6e), [`set-entry-on-cursor-change-callback`](#link7365742d656e7472792d6f6e2d637572736f722d6368616e67652d63616c6c6261636b), [`get-entry-cursor`](#link6765742d656e7472792d637572736f72), [`get-entry-selected-text`](#link6765742d656e7472792d73656c65637465642d74657874), [`set-entry-min-rows-visible`](#link7365742d656e7472792d6d696e2d726f77732d76697369626c65), [`set-entry-place-holder`](#link7365742d656e7472792d706c6163652d686f6c646572), [`set-entry-text`](#link7365742d656e7472792d74657874).	 [→index](#idx)

### `new-form` : procedure/0

Usage: `(new-form)`

Return a new form container, which orders widgets in rows, where each row has a label and a widget whose columns are aligned with the other rows. Use append-form to add label and widgets.

See also: [`append-form`](#link617070656e642d666f726d).	 [→index](#idx)

### `new-form-item` : procedure/3

Usage: `(new-form-item text widget hint-text) => int`

Create a new form item. Such an item can be used in `show-form` dialogs to display forms. For adding items to a form based on `new-form`, use `append-form` to append text and strings to forms instead.

See also: [`show-form`](#link73686f772d666f726d).	 [→index](#idx)

### `new-form-layout` : procedure/0

Usage: `(new-form-layout) => int`

Create a form layout, which arranges elements in two columns per row, where the columns are aligned.

See also: [`new-form`](#link6e65772d666f726d), [`append-form`](#link617070656e642d666f726d), [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx)

### `new-grid-layout` : procedure/1

Usage: `(new-grid-layout n) => int`

Create a new grid layout, which arranges elements in `n` columns.

See also: [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx)

### `new-grid-wrap-layout` : procedure/2

Usage: `(new-grid-wrap-layout width height) => int`

Create a new grid wrap layout, which arranges elements such that each element has the given `width` and `height`, and wraps lines based on the size of the parent container.

See also: [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx)

### `new-hbox-layout` : procedure/0

Usage: `(new-hbox-layout) => int`

Create a new horizontal box layout, which lays out container elements horizontally.

See also: [`new-spacer`](#link6e65772d737061636572), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx)

### `new-hscroll` : procedure/1

Usage: `(new-hscroll obj) => int`

Embed canvas object `obj` into a new horizontal scroll container, which allows the user to scroll horizontally if `obj` does not fit into the hscroll container horizontally.

See also: [`new-scroll`](#link6e65772d7363726f6c6c), [`new-vscroll`](#link6e65772d767363726f6c6c), [`new-container`](#link6e65772d636f6e7461696e6572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574).	 [→index](#idx)

### `new-hsplit` : procedure/2

Usage: `(new-hsplit obj1 obj2) => int`

Return a new horizontal divider between canvas object `obj1` and `obj2`. The user can adjust the division by drag & drop.

See also: [`set-split-offset`](#link7365742d73706c69742d6f6666736574), [`new-vsplit`](#link6e65772d7673706c6974).	 [→index](#idx)

### `new-hyperlink` : procedure/2

Usage: `(new-hyperlink label url) => int`

Create a new hyperlink with given `label` string and an `url` string as embedded link. A hyperlink looks like a label with link style; when it is clicked, the `url` is called by the default operating system mechanism for opening urls. Using hyperlinks might be disallowed by the host system configuration HyperlinksAllowed and may be re-written by the host system using the CheckHyperlinks function. If HyperlinksAllowed is false in the active GUI config of the host, this function raises an error. It also parses the given `url` and will raise an error if it does not represent a valid URL.

See also: [`new-button`](#link6e65772d627574746f6e), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

**Warning: Allowing the host to open hyperlinks usually launches a web browser and the details depend on the operating system. There is an added security risk!**

### `new-icon` : procedure/1

Usage: `(new-icon resource) => int`

Create a new icon from `resource`, which must be suitable to create an image.

See also: [`theme-icon`](#link7468656d652d69636f6e).	 [→index](#idx)

### `new-image-from-file` : procedure/1

Usage: `(new-image-from-file path) => int`

Create and return a new image from the image file at `path`, which must be a PNG file.

See also: [`new-image-from-resource`](#link6e65772d696d6167652d66726f6d2d7265736f75726365).	 [→index](#idx)

### `new-image-from-resource` : procedure/1

Usage: `(new-image-from-resource resource) => int`

Create and return a new image from the given `resource.`

See also: [`new-image-from-file`](#link6e65772d696d6167652d66726f6d2d66696c65), [`theme-icon`](#link7468656d652d69636f6e).	 [→index](#idx)

### `new-label` : procedure/1

Usage: `(new-label str) => int`

Creates a new text label with string `str.`

See also: [`set-label-text`](#link7365742d6c6162656c2d74657874).	 [→index](#idx)

### `new-line` : procedure/1 or more

Usage: `(new-line fill-color [pos1] [pos2] [stroke-color] [stroke-width]) => int`

Draw and return a line with the given NRGBA `fill-color` from optional position `pos1` to position `pos2`, where these are lists of `x` and `y` coordinates as floats. The optional `stroke-color` and `stroke-width` determines the outer edges of the line.

See also: [`new-cirlce`](#link6e65772d6369726c6365), [`new-rectangle`](#link6e65772d72656374616e676c65), [`new-text`](#link6e65772d74657874).	 [→index](#idx)

### `new-list` : procedure/3

Usage: `(new-list len-proc prep-proc update-proc) => int`

Create a new list display. A list consists of rows of simple items like labels. The `len-proc` must be a procedure without arguments returning the length of the list as integer. The `prep-proc` must be a procedure without arguments that returns a canvas object (i.e. a label or other widgets) representing a template for a single list item. The `update-proc` must be a procedure that receives the ID of a canvas object (given by the template) and the 0-based list index as arguments. This procedure then should modify the canvas object with `ID` to display the given list item at the index. See the GUI examples on how to use this function.

See also: [`new-table`](#link6e65772d7461626c65), [`new-tree`](#link6e65772d74726565).	 [→index](#idx)

### `new-main-menu` : procedure/1 or more

Usage: `(new-main-menu menu ...) => int`

Return a new main menu with the given menus. A main menu displays a menubar for a window on some desktop platforms but it may also be displayed in other ways.

See also: [`new-menu`](#link6e65772d6d656e75), [`new-menu*`](#link6e65772d6d656e752a).	 [→index](#idx)

### `new-menu` : procedure/1

Usage: `(new-menu menu*) => int`

Create a new visible menu widget from the abstract `menu*` created by new-menu*.

See also: [`new-menu*`](#link6e65772d6d656e752a), [`new-main-menu`](#link6e65772d6d61696e2d6d656e75).	 [→index](#idx)

### `new-menu*` : procedure/1 or more

Usage: `(new-menu* label [item...]) => int`

Make a new abstract menu with given `label` and arbitary menu items `item` ... following. The starred function is used to define a menu but is not bound to any particular way of displaying it (popup-menu, normal menu, main menu). Use `new-menu` and `new-main-menu` to create visible menus and menu bars based on such abstract menus.

See also: [`refresh-menu*`](#link726566726573682d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-main-menu`](#link6e65772d6d61696e2d6d656e75).	 [→index](#idx)

### `new-menu-item` : procedure/2 or more

Usage: `(new-menu-item str proc [selector...]) => int`

Create a new menu item with given label `str` and callback `proc`, which takes no arguments. The optional `selector` symbol may be one of: 'is-quit - the item is the application Quit menu item (this is dealt with differently by operating system requirements), 'is-separator - the item is a menu item separator and the label string is ignored (redundent, use `new-menu-item-separator` instead), 'disabled - the menu item is disabled, or 'checked - the menu item is checked.

See also: [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`menu-item-disabled?`](#link6d656e752d6974656d2d64697361626c65643f), [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx)

### `new-menu-item-separator` : procedure/0

Usage: `(new-menu-item-separator) => int`

Return a new menu item separator, which is a menu item without callback and label that displays a separator between menu items in menus.

See also: [`new-menu-item`](#link6e65772d6d656e752d6974656d).	 [→index](#idx)

### `new-progress-bar` : procedure/0

Usage: `(new-progress-bar) => int`

Create a new progress bar whose default minimum is 0.0 and maximum is 1.0.

See also: [`set-progress-bar`](#link7365742d70726f67726573732d626172), [`new-infinite-progress-bar`](#link6e65772d696e66696e6974652d70726f67726573732d626172), [`get-progress-bar-value`](#link6765742d70726f67726573732d6261722d76616c7565).	 [→index](#idx)

### `new-raster-with-pixels` : procedure/1

Usage: `(new-raster-with-pixels pixel-proc) => int`

Create a new raster image generated dynamically by the given `pixel-proc`. The `pixel-proc` takes `x` and `y` pixel coordinates and the `width` and `height` of the image in pixels, and returns the color of the pixel `x`, `y` as a color list of the form '(red green blue [alpha]) where `alpha` is optional. Notice that specifying the color of each pixel can be very CPU-intensive for larger images, so optimizations might be necessary.

See also: [`new-image-from-file`](#link6e65772d696d6167652d66726f6d2d66696c65).	 [→index](#idx)

### `new-rectangle` : procedure/1 or more

Usage: `(new-rectangle fill-color [width height] [position] [stroke-color] [stroke-width] [corner-radius]) => int`

Draw and return a rectangle with the given NRGBA `fill-color`. The optional int `width` and `height` arguments set the width and height of the rectangle explicitly (otherwise they are 1). The optional `position` argument must be a list of `x` and `y` coordinates as floats. The optional `stroke-color` and `stroke-width` arguments determine the color and width of the outline of the rectangle, and the optional `corner-radious` defines how rounded the rectangle is. Notice that the rectangle's size and position can be set by the layout of the container, so to set it manually you need to make sure the underlying container has no layout that positions or resizes the rectangle.

See also: [`new-circle`](#link6e65772d636972636c65), [`new-line`](#link6e65772d6c696e65), [`new-text`](#link6e65772d74657874).	 [→index](#idx)

### `new-regexp-validator` : procedure/2

Usage: `(new-regexp-validator regexp reason) => int`

Create a new string validator from the `regexp` string, which must be a valid regular expression in Go's regexp syntax. The `reason` string is displayed to the user by widgets like `entry` when the validation fails.

See also: [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72), [`new-validator`](#link6e65772d76616c696461746f72), [`new-combined-string-validator`](#link6e65772d636f6d62696e65642d737472696e672d76616c696461746f72), [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72), [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b), [`validate-object`](#link76616c69646174652d6f626a656374).	 [→index](#idx)

### `new-scroll` : procedure/1

Usage: `(new-scroll obj) => int`

Embed canvas object `obj` into a new scroll container, which allows the user to scroll both horizontally and vertically if `obj` does not fit into the scroll container.

See also: [`new-vscroll`](#link6e65772d767363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c), [`new-container`](#link6e65772d636f6e7461696e6572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574).	 [→index](#idx)

### `new-slider` : procedure/3

Usage: `(new-slider min max proc) => int`

Create a new slider that allows users to adjust numerical values. The `min` and `max` arguments must be floats. The procedure `proc` takes the current slider float value as argument and is called when the slider changes.

See also: [`set-slider-value`](#link7365742d736c696465722d76616c7565).	 [→index](#idx)

### `new-spacer` : procedure/0

Usage: `(new-spacer) => int`

Create a new spacer, which adjusts size dynamically by taking up space and displaying nothing. Use this to fill containers e.g. to right align a widget.

See also: [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx)

### `new-stack-layout` : procedure/0

Usage: `(new-stack-layout) => int`

Create a new stack layout that stacks container elements on top of each other, overlapping. This may be used for drawing, for example.

See also: [`new-form`](#link6e65772d666f726d), [`append-form`](#link617070656e642d666f726d), [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx)

### `new-tabitem` : procedure/2

Usage: `(new-tabitem title obj) => int`

Create a new tab item for use in app-tabs and doc-tabs with a `title` and an embedded canvas object `obj` shown when the tab item is selected in the tabs.

See also: [`new-tabitem-with-icon`](#link6e65772d7461626974656d2d776974682d69636f6e), [`new-app-tabs`](#link6e65772d6170702d74616273), [`new-doc-tabs`](#link6e65772d646f632d74616273).	 [→index](#idx)

### `new-tabitem-with-icon` : procedure/3

Usage: `(new-tabitem-with-icon title icon obj) => int`

Create a new tab item for use in app-tabs and doc-tabs with given `title` string, `icon` resource, and embedded canvas object `obj` that shwon when the tab item is selected in the tabs.

See also: [`new-tabitem`](#link6e65772d7461626974656d), [`new-app-tabs`](#link6e65772d6170702d74616273), [`new-doc-tabs`](#link6e65772d646f632d74616273).	 [→index](#idx)

### `new-table` : procedure/3

Usage: `(new-table len-proc prep-proc update-proc) => int`

Create a new table display. A table consists of a number of rows, each of which has a fixed number of columns such as labels. The `len-proc` must be a procedure without arguments returning the length of the table as integer. The `prep-proc` must be a procedure without arguments that returns a canvas object that represents the table row with updatable columns. The `update-proc` takes the row, column, and ID of a canvas object and updates a table template with the right display for the table cell at `row` and `column.`

See also: [`new-list`](#link6e65772d6c697374), [`new-tree`](#link6e65772d74726565).	 [→index](#idx)

### `new-text` : procedure/2

Usage: `(new-text str color) => int`

Draw and return text with the given string `str` and foreground NRGBA `color.`

See also: [`set-text-alignment`](#link7365742d746578742d616c69676e6d656e74), [`set-text-size`](#link7365742d746578742d73697a65), [`set-text-style`](#link7365742d746578742d7374796c65), [`new-line`](#link6e65772d6c696e65), [`new-cirle`](#link6e65772d6369726c65), [`new-rectangle`](#link6e65772d72656374616e676c65).	 [→index](#idx)

### `new-text-grid` : procedure/0 or more

Usage: `(new-text-grid [<string>] [show-line-numbers|show-whitespace|tab-width <int>]) => int`

Create a new text grid widget, which displays multiline text with custom background and foreground colors. The optional `string` argument is the initial text of the grid without formatting. The following symbols might be `'show-line-numbers` to turn the line number display on and `'show-whitespace` to display white space characters by special unicode symbols. If the selector `'tab-width` occurs, then it must be immediately followed by an integer for the tabulator width of the text grid in space characters.

See also: [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874), [`remove-text-grid-row`](#link72656d6f76652d746578742d677269642d726f77), [`insert-text-grid-row`](#link696e736572742d746578742d677269642d726f77).	 [→index](#idx)

### `new-time-validator` : procedure/1

Usage: `(new-time-validator format-str) => int`

Create a new string validator for time and date based on the given template `format-str`. This validator uses Go's data parsing function and therefore is quite restrictive. Only datetimes entered in exactly the format given (including timezones) validate successfully. To obtain a more relaxed date and time validator, use `new-validator` to create a custom validator with your own parsing or try a `new-regexp-validator.`

See also: [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72), [`new-validator`](#link6e65772d76616c696461746f72), [`new-combined-string-validator`](#link6e65772d636f6d62696e65642d737472696e672d76616c696461746f72), [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72), [`new-regexp-validator`](#link6e65772d7265676578702d76616c696461746f72), [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b), [`validate-object`](#link76616c69646174652d6f626a656374).	 [→index](#idx)

### `new-tree` : procedure/4

Usage: `(new-tree child-uid-proc is-branch-proc create-node-proc update-node-proc) => int`

Create a new tree display. A tree displays nested branches and leaf nodes. The `child-uid-proc` is a procedure that takes an id string as argument. If the string is empty, it should return a list of top-level branch uid strings. If the string is not empty, it represents an uid; the procedure should then return a list of all child id strings of that branch. This defines the tree's structure. All id strings must be unique to the tree. The `is-branch-proc` takes an id string as argument and should return non-nil if the id represents a branch, nil if it has no children. The `create-node-proc` takes a bool `b` as argument and should return a branch template if `b` is non-nil and a leaf template object if `b` is nil. Finally, the `update-node-proc` is a procedure that takes a node id string, a boolean that is true if the node is a branch, and a node template canvas-object as it is returned by `create-node-proc`. The procedure should fill the template with the display values for the respective node id.

See also: [`new-list`](#link6e65772d6c697374), [`new-table`](#link6e65772d7461626c65).	 [→index](#idx)

### `new-validator` : procedure/1

Usage: `(new-validator proc) => int`

Create a new string validator based on validation procedure `proc`. The procedure `proc` takes a string as argument and returns a string. If the string returned is not the empty string "", then validation fails and the returned string is given as a reason for validation failure. If the empty string is returned, then validation succeeds. If an error occurs in `proc`, then validation fails with the error's error message as reason. Notice that validators are fairly limited and can only be attached to a few validatable objects such as text entry fields. For a more general approach, it might make sense to implement your own validation system based on key press, focus change, and change callbacks of various GUI objects.

See also: [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72), [`new-combined-string-validator`](#link6e65772d636f6d62696e65642d737472696e672d76616c696461746f72), [`new-regexp-validator`](#link6e65772d7265676578702d76616c696461746f72), [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72), [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b), [`validate-object`](#link76616c69646174652d6f626a656374).	 [→index](#idx)

### `new-vbox-layout` : procedure/0

Usage: `(new-vbox-layout) => int`

Create a new vertical box layout, which lays out container elements vertically.

See also: [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx)

### `new-vscroll` : procedure/1

Usage: `(new-vscroll obj) => int`

Embed canvas object `obj` into a new vertical scroll container, which allows the user to scroll vertically if `obj` does not fit into the vscroll container vertically.

See also: [`new-scroll`](#link6e65772d7363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c), [`new-container`](#link6e65772d636f6e7461696e6572), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574).	 [→index](#idx)

### `new-vsplit` : procedure/2

Usage: `(new-vsplit obj1 obj2) => int`

Return a new vertical divider between canvas object `obj1` and `obj2`. The user can adjust the division by drag & drop.

See also: [`set-split-offset`](#link7365742d73706c69742d6f6666736574), [`new-hplit`](#link6e65772d68706c6974).	 [→index](#idx)

### `new-window` : procedure/1

Usage: `(new-window title) => int`

Create a new window with `title` string and return the window ID. This function raises an error if the host configuration WindowsAllowed is not true. In certain embedded uses, creating new windows is not allowed and you should check the documentation how to find a pre-configured window and add user interface elements to it.

See also: [`set-window-content`](#link7365742d77696e646f772d636f6e74656e74), [`close-window`](#link636c6f73652d77696e646f77), [`show-window`](#link73686f772d77696e646f77).	 [→index](#idx)

### `new-zedit` : procedure/3

Usage: `(new-zedit columns lines canvas) => zedit`

Create a new zedit editor widget, which allows for source code editing and advanced syntax coloring but has other restrictions and does not support full text syles or embedded images.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`new-entry`](#link6e65772d656e747279).	 [→index](#idx)

### `nrgba` : procedure/4

Usage: `(nrgba red green blue alpha) => int`

Create an RGBA color where `red`, `green`, `blue`, and `alpha` are 8-bit uint integers, i.e., values between 0 and 255 (inclusive). Notice that some GUI functions require NRGBA color returned by this function, whereas others require a color list of int values '(red green blue alpha). This is for performance reasons, since it sometimes faster to convert a list to a color on-the-fly and sometimes more convenient to store pre-defined colors for later re-use.

See also: [`nrgba64`](#link6e726762613634), [`theme-color`](#link7468656d652d636f6c6f72), [`new-rectangle`](#link6e65772d72656374616e676c65), [`new-circle`](#link6e65772d636972636c65), [`new-line`](#link6e65772d6c696e65), [`new-text`](#link6e65772d74657874).	 [→index](#idx)

### `nrgba64` : procedure/4

Usage: `(nrgba64 red green blue alpha) => int`

Create a 64-bit RGBA color where `red`, `green`, `blue`, and `alpha` are 16-bit uint integers, i.e., values between 0 and 65365 (inclusive). Notice that some GUI functions require NRGBA64 color returned by this function, whereas others require a color list of int values '(red green blue alpha). This is for performance reasons, since it sometimes faster to convert a list to a color on-the-fly and sometimes more convenient to store pre-defined colors for later re-use.

See also: [`nrgba`](#link6e72676261), [`theme-color`](#link7468656d652d636f6c6f72), [`new-rectangle`](#link6e65772d72656374616e676c65), [`new-circle`](#link6e65772d636972636c65), [`new-line`](#link6e65772d6c696e65), [`new-text`](#link6e65772d74657874).	 [→index](#idx)

### `object-disabled?` : procedure/1

Usage: `(object-disabled? obj) => bool`

Return true if the canvas object `obj` is disabled, nil otherwise.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `print-zedit` : procedure/3

Usage: `(print-zedit zedit s tags)`

Print text `s` at the current caret position and apply `tags` to the printed text.

See also: [`new-zedit`](#link6e65772d7a65646974), [`insert-zedit`](#link696e736572742d7a65646974), [`delete-zedit`](#link64656c6574652d7a65646974).	 [→index](#idx)

### `refresh-main-menu` : procedure/1

Usage: `(refresh-main-menu main-menu)`

Refresh the given `main-menu` display. This should be called after some submenus or menu items in the main menu have changed.

See also: [`new-main-menu`](#link6e65772d6d61696e2d6d656e75), [`refresh-menu*`](#link726566726573682d6d656e752a).	 [→index](#idx)

### `refresh-menu*` : procedure/1

Usage: `(refresh-menu* menu)`

Refresh the given `menu` after a change was made that has a visual impact. This will refresh the menu widget in which this abstract menu occurs.

See also: [`refresh-main-menu`](#link726566726573682d6d61696e2d6d656e75), [`new-menu*`](#link6e65772d6d656e752a).	 [→index](#idx)

### `refresh-object` : procedure/1

Usage: `(refresh-object obj)`

Refresh the canvas object `obj`, causing the graphical display to be re-drawn as soon as possible. This may be needed if the object's state has changed.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `remove-canvas-shortcut` : procedure/2

Usage: `(remove-canvas-shortcut canvas shortcut)`

Remove the `shortcut` from `canvas`, where `shortcut` is a list consisting of valid keyboard modifier symbols and a valid key symbol.

See also: [`add-canvas-shortcut`](#link6164642d63616e7661732d73686f7274637574), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173).	 [→index](#idx)

### `remove-text-grid-row` : procedure/2

Usage: `(remove-text-grid-row grid row)`

Remove the `row` from the given text `grid`. An incorrect `row` index will result in an error.

See also: [`insert-text-grid-row`](#link696e736572742d746578742d677269642d726f77), [`new-text-grid`](#link6e65772d746578742d67726964), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77).	 [→index](#idx)

### remove-zedit-event-handler : nil

Usage: `(remove-zedit-event-handler editor sel)`

Remove the event handler for `sel` in `editor`, do nothing if no handler has been set for the event. The selector `sel` must be one of '(caret-move).

See also: [`new-zedit`](#link6e65772d7a65646974), [`set-zedit-event-handler`](#link7365742d7a656469742d6576656e742d68616e646c6572).	 [→index](#idx)

### `resize-object` : procedure/2

Usage: `(resize-object obj width height)`

Resize canvas object `obj` to the given `width` and `height` as floats.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `set-app-metadata` : procedure/7

Usage: `(set-app-metadata id name version build icon release? custom)`

Set the metadata of the application to the `id` string, `name` string, `version` string, `build` integer, `icon` resource ID, `release?` bool if this version is a release rather than for testing, and a `custom` dict containing string to string key-value mappings.

See also: [`new-window`](#link6e65772d77696e646f77).	 [→index](#idx)

### `set-canvas-on-typed-key` : procedure/2

Usage: `(set-canvas-on-typed-key canvas proc)`

Set the procedure `proc` called when a key is typed in `canvas`. `proc` takes two arguments, the first one is a platform-independent key symbol and the second one is a platform- and keyboard-dependent hardware scancode.

See also: [`set-canvas-on-typed-rune`](#link7365742d63616e7661732d6f6e2d74797065642d72756e65), [`add-canvas-shortcut`](#link6164642d63616e7661732d73686f7274637574), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173).	 [→index](#idx)

### `set-canvas-on-typed-rune` : procedure/2

Usage: `(set-canvas-on-typed-rune canvas proc)`

Set the procedure `proc` called when a rune is typed in `canvas`. `proc` takes one argument, a string containing a single Unicode rune.

See also: [`add-canvas-shortcut`](#link6164642d63616e7661732d73686f7274637574), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`set-canvas-on-typed-key`](#link7365742d63616e7661732d6f6e2d74797065642d6b6579).	 [→index](#idx)

### `set-clipboard-content` : procedure/1

Usage: `(set-clipboard-content str)`

Set the operating system clipboard content to string `str`. This function might raise an error if clipboard access is prohibited by host security settings.

See also: [`get-clipboard-content`](#link6765742d636c6970626f6172642d636f6e74656e74).	 [→index](#idx)

### `set-entry-cursor-column` : procedure/2

Usage: `(set-entry-cursor-column entry column)`

Set the column position of the cursor in `entry` to integer `column.`

See also: [`get-entry-cursor-pos`](#link6765742d656e7472792d637572736f722d706f73), [`set-entry-cursor-row`](#link7365742d656e7472792d637572736f722d726f77).	 [→index](#idx)

### `set-entry-cursor-row` : procedure/2

Usage: `(set-entry-cursor-row entry row)`

Set the row position of the cursor in `entry` to integer `row.`

See also: [`get-entry-cursor-pos`](#link6765742d656e7472792d637572736f722d706f73), [`set-entry-cursor-column`](#link7365742d656e7472792d637572736f722d636f6c756d6e).	 [→index](#idx)

### `set-entry-min-rows-visible` : procedure/2

Usage: `(set-entry-min-rows-visible entry rows)`

Set the minimum number of rows of `entry` that are visible. This ensures that `rows` text rows are visible and is a way of setting the entry's minimum size. Curiously, there is no corresponding set-entry-min-columns-visible function yet.

See also: [`new-entry`](#link6e65772d656e747279).	 [→index](#idx)

### `set-entry-on-change-callback` : procedure/2

Usage: `(set-entry-on-change-callback entry proc)`

Set the callback of `entry` that is triggered when the entry text changes. `proc` must be a procedure that takes the entry text as string.

See also: [`new-entry`](#link6e65772d656e747279), [`set-entry-cursor-change-callback`](#link7365742d656e7472792d637572736f722d6368616e67652d63616c6c6261636b).	 [→index](#idx)

### `set-entry-on-cursor-change-callback` : procedure/2

Usage: `(set-entry-cursor-change-callback entry proc)`

Set the cursor change callback of `entry` to `proc`, which is a procedure that takes the entry ID as argument.

See also: [`new-entry`](#link6e65772d656e747279), [`set-entry-on-change-callback`](#link7365742d656e7472792d6f6e2d6368616e67652d63616c6c6261636b).	 [→index](#idx)

### `set-entry-place-holder` : procedure/2

Usage: `(set-entry-place-holder entry str)`

Set the place holder string of `entry` to `str`. This is displayed as a prompt when no text is entered.

See also: [`new-entry`](#link6e65772d656e747279), [`set-entry-text`](#link7365742d656e7472792d74657874).	 [→index](#idx)

### `set-entry-text` : procedure/2

Usage: `(set-entry-text entry str)`

Set the text of `entry` to string `str.`

See also: [`new-entry`](#link6e65772d656e747279), [`set-entry-place-holder`](#link7365742d656e7472792d706c6163652d686f6c646572).	 [→index](#idx)

### `set-entry-text-wrap` : procedure/2

Usage: `(set-entry-text-wrap entry selector)`

Set or remove the text wrapping of `entry`, which is only relevant for multiline entries. `selector` must be one of '(none break wrap), where 'none indicates no text wrapping, 'break indicates that words are broken without special wrapping algorithm, and 'word means word wrapping.

See also: [`new-entry`](#link6e65772d656e747279).	 [→index](#idx)

### `set-entry-validator` : procedure/2

Usage: `(set-entry-validator entry validator)`

Set the `validator` of `entry`. A validator must be created first from a special procedure or a regular expression.

See also: [`new-entry`](#link6e65772d656e747279), [`new-validator`](#link6e65772d76616c696461746f72), [`new-combined-string-validator`](#link6e65772d636f6d62696e65642d737472696e672d76616c696461746f72), [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72), [`new-regexp-validator`](#link6e65772d7265676578702d76616c696461746f72), [`validate-object`](#link76616c69646174652d6f626a656374).	 [→index](#idx)

### `set-label-text` : procedure/2

Usage: `(set-label-text label str)`

Sets the text of `label` to string `str`. This might resize the label depending on the layout in which the label is put.

See also: [`get-label-text`](#link6765742d6c6162656c2d74657874), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `set-menu-item-checked` : procedure/2

Usage: `(set-menu-item-checked item checked?)`

Set the menu item check mark display if `checked?` is non-nil, remove it otherwise.

See also: [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`menu-item-disabled?`](#link6d656e752d6974656d2d64697361626c65643f), [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx)

### `set-menu-item-disabled` : procedure/2

Usage: `(set-menu-item-disabled item disabled?)`

Disable the menu item if `disabled?` is non-nil, enable it otherwise.

See also: [`menu-item-disabled?`](#link6d656e752d6974656d2d64697361626c65643f), [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx)

### `set-menu-item-label` : procedure/2

Usage: `(set-menu-item-label item str)`

Set the label of menu `item` to `str.`

See also: [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`menu-item-disabled`](#link6d656e752d6974656d2d64697361626c6564), [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx)

### `set-object-on-validation-change-callback` : procedure/2

Usage: `(set-object-on-validation-change-callback obj proc)`

Set a validatable object's `obj` validation change callback, which is called when the object's validation changes. The callback `proc` takes a string or nil as argument. When it is nil, the validation was successful. When it is a string, then the validation failed with the string as reason. This can be used to track validation changes of any validatable object (such as a text entry) to e.g. display custom messages or icons when the validation fails or succeeds.

See also: [`validate-object`](#link76616c69646174652d6f626a656374), [`new-validator`](#link6e65772d76616c696461746f72), [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72).	 [→index](#idx)

### `set-progress-bar` : procedure/1 or more

Usage: `(set-progress-bar bar value [selector value])`

Set the value of progress-bar `bar` as follows. If a single number is provided, then the current value of the progress-bar is set to this number. If a selector symbol is provided, then if it is 'value, the progress-bar value is set to the following number, if it is 'max or 'min, then the progress-bar maximum or minimum values are set to the respective following number. If it is 'formatter, then the following value must be a procedure that takes the progress-bar ID as argument and returns a string that represents the display of the progress-bar at the given time.

See also: [`get-progress-bar-value`](#link6765742d70726f67726573732d6261722d76616c7565), [`new-progress-bar`](#link6e65772d70726f67726573732d626172), [`new-infinite-progress-bar`](#link6e65772d696e66696e6974652d70726f67726573732d626172).	 [→index](#idx)

### `set-scroll-offset` : procedure/2

Usage: `(set-scroll-offset scroll li)`

Set the `scroll` offset to `li`, which is a position of the form (x y) where `x` and `y` are floats. If you don't want to change `x` or `y` respectively, you need to use `get-scroll-offset` first to get the value that you don't want to change, and construct the position from that.

See also: [`get-scroll-offset`](#link6765742d7363726f6c6c2d6f6666736574), [`new-scroll`](#link6e65772d7363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c), [`new-vscroll`](#link6e65772d767363726f6c6c).	 [→index](#idx)

### `set-select-options` : procedure/2

Usage: `(set-select-options select li)`

Set the options of the given `select` widget, which can be obtained using `new-choice` with selector 'select.

See also: [`new-choice`](#link6e65772d63686f696365).	 [→index](#idx)

### `set-slider-value` : procedure/2

Usage: `(set-slider-value slider fl)`

Set the value of `slider` to float `fl.`

See also: [`new-slider`](#link6e65772d736c69646572).	 [→index](#idx)

### `set-split-offset` : procedure/2

Usage: `(set-split-offset split offset)`

Set the offset of `split` to float `offset` between 0.0 and 1.0. `offset` indicates the percentage between the objects shown in the split. If `offset` is 0.0, then only the second object is shown, if it is 1.0 then only the first object is shown.

See also: [`new-vsplit`](#link6e65772d7673706c6974), [`new-hsplit`](#link6e65772d6873706c6974).	 [→index](#idx)

### `set-text-alignment` : procedure/2

Usage: `(set-text-alignment text sym)`

Set the alignment of `text` to `sym`, which must be one of '(leading center trailing).

See also: [`new-text`](#link6e65772d74657874), [`set-text-size`](#link7365742d746578742d73697a65), [`set-text-style`](#link7365742d746578742d7374796c65).	 [→index](#idx)

### `set-text-grid-cell` : procedure/4

Usage: `(set-text-grid-cell grid row column li)`

Set the text `grid` cell at `row` and `column` (both 0-indexed) to the list `li`, where `li` must consist of a unicode string containing one rune and a valid grid style list.

See also: [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77).	 [→index](#idx)

### `set-text-grid-row` : procedure/3

Usage: `(set-text-grid-row grid row row-spec)`

Set the `row` of `grid` to the given `row-spec`, which is a list containing an array of grid cells like in the return value of `get-text-grid-row` and a grid style for the row as a whole.

See also: [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765).	 [→index](#idx)

### `set-text-grid-row-style` : procedure/3

Usage: `(set-text-grid-row-style grid row style)`

Set the style of text `grid` at `row` to the given grid `style.`

See also: [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765).	 [→index](#idx)

### `set-text-grid-rune` : procedure/4

Usage: `(set-text-grid-rune grid row column str)`

Set the rune of `grid` at `row` and `column` to the unicode glyph in string `str.`

See also: [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c).	 [→index](#idx)

### `set-text-grid-show-line-numbers` : procedure/2

Usage: `(set-text-grid-show-line-numbers grid show?)`

Set whether `grid` shows line numbers. If `show?` is not nil, then line numbers are shown, otherwise they are not shown.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx)

### `set-text-grid-show-whitespace` : procedure/2

Usage: `(set-text-grid-show-whitespace grid show?)`

Set whether `grid` shows whitespace characters. If `show?` is not nil, then whitespace characters are shown, otherwise they are not shown.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx)

### `set-text-grid-style` : procedure/4

Usage: `(set-text-grid-style grid row column style)`

Set the grid style of `grid` at `row` and `column` to the a-list `style.`

See also: [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765).	 [→index](#idx)

### `set-text-grid-style-range` : procedure/6

Usage: `(set-text-grid-style-range grid start-row start-column end-row end-column style)`

Set the grid style of `grid` starting at `start-row` and `start-column` and ending at `end-row` and `end-column` (all inclusive) to the grid `style.`

See also: [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65).	 [→index](#idx)

### `set-text-grid-tab-width` : procedure/2

Usage: `(set-text-grid-tab-width grid width)`

Set the tabulator width of `grid` to integer `width` space characters.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx)

### `set-text-grid-text` : procedure/2

Usage: `(set-text-grid-text grid str)`

Set the text of the text `grid` to the given `str.`

See also: [`get-text-grid-text`](#link6765742d746578742d677269642d74657874), [`new-text-grid`](#link6e65772d746578742d67726964), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77).	 [→index](#idx)

### `set-text-size` : procedure/2

Usage: `(set-text-size text size)`

Set the size of `text` to float `size.`

See also: [`new-text`](#link6e65772d74657874), [`set-text-alignment`](#link7365742d746578742d616c69676e6d656e74), [`set-text-style`](#link7365742d746578742d7374796c65).	 [→index](#idx)

### `set-text-style` : procedure/2

Usage: `(set-text-style text li`

Set the style of `text` to the specification in list `li`, which must contain symbols in '(bold italic monospace symbol tab-width). If a symbol in the list is `tab-width`, it must be followed by an integer. `bold` sets boldface, `italic` makes the style italic, `monospace` selects the monospace/typewriter font, and `symbol` selects the `symbol` font. `tab-width` followed by an integer sets the width of tabulator in terms of the number of space characters.

See also: [`new-text`](#link6e65772d74657874), [`set-text-alignment`](#link7365742d746578742d616c69676e6d656e74), [`set-text-size`](#link7365742d746578742d73697a65).	 [→index](#idx)

### `set-window-content` : procedure/2

Usage: `(set-window-content window canvas-object)`

Set the main content of the window, which must be an existing canvas object such as a widget or container with layout.

See also: [`get-window-content`](#link6765742d77696e646f772d636f6e74656e74), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`new-window`](#link6e65772d77696e646f77).	 [→index](#idx)

### `set-window-fixed-size` : procedure/2

Usage: `(set-window-fixed-size window fixed-size?)`

If `fixed-size?` is not nil, then `window` is set to fixed size, i.e., it has no resize button and cannot be resized by the user; otherwise, the window is set to being resizable.

See also: [`window-fixed-size?`](#link77696e646f772d66697865642d73697a653f).	 [→index](#idx)

### `set-window-full-screen` : procedure/2

Usage: `(set-window-full-screen window full-screen?)`

If `full-screen?` is not nil, then `window` is set to full screen mode, otherwise the window is set to normal mode. In full screen mode the window is centered and fills the whole screen of the main monitor (multiple monitors are currently not supported).

See also: [`window-full-screen?`](#link77696e646f772d66756c6c2d73637265656e3f), [`center-window-on-screen`](#link63656e7465722d77696e646f772d6f6e2d73637265656e).	 [→index](#idx)

### `set-window-icon` : procedure/1

Usage: `(set-window-icon window resource)`

Set the icon of `window` to the given icon `resource`. Setting the icon does not guarantee that it is displayed, since this is platform-dependent.

See also: [`get-window-icon`](#link6765742d77696e646f772d69636f6e), [`new-icon`](#link6e65772d69636f6e), [`theme-icon`](#link7468656d652d69636f6e).	 [→index](#idx)

### `set-window-main-menu` : procedure/2

Usage: `(set-window-main-menu window main-menu)`

Set the main menu of `window` to `main-menu.`

See also: [`new-main-menu`](#link6e65772d6d61696e2d6d656e75), [`new-menu`](#link6e65772d6d656e75), [`new-menu*`](#link6e65772d6d656e752a).	 [→index](#idx)

### `set-window-on-close-callback` : procedure/2

Usage: `(set-window-on-close-callback window proc)`

Sets the close callback of `window`. `proc` must be a function that takes no arguments and is evaluated when the window is closed.

See also: [`show-window`](#link73686f772d77696e646f77), [`close-window`](#link636c6f73652d77696e646f77), [`hide-window`](#link686964652d77696e646f77).	 [→index](#idx)

### `set-window-padded` : procedure/2

Usage: `(set-window-padded window padded?)`

If `padded?` is not nil, then `window` is set to being padded. This is the default of new window. If `padded?` is nil, then the window's padding is removed, which means that the whole content area of the window can be filled with user interface elements and draw commands. This would be used for a game display, for instance.

See also: [`window-padded?`](#link77696e646f772d7061646465643f), [`set-window-full-screen`](#link7365742d77696e646f772d66756c6c2d73637265656e), [`window-full-screen?`](#link77696e646f772d66756c6c2d73637265656e3f), [`center-window-on-screen`](#link63656e7465722d77696e646f772d6f6e2d73637265656e).	 [→index](#idx)

### `set-window-size` : procedure/2

Usage: `(set-window-size window width height)`

Set the size of `window` to `width` and `height` as floats. Sizes and positions are generally given as floats whose accuracy is only guaranteed to 32 bit.

See also: [`new-window`](#link6e65772d77696e646f77), [`show-window`](#link73686f772d77696e646f77), [`hide-window`](#link686964652d77696e646f77).	 [→index](#idx)

### `set-window-title` : procedure/2

Usage: `(set-window-title window title)`

Set the title of `window` to string `title.`

See also: [`get-window-title`](#link6765742d77696e646f772d7469746c65).	 [→index](#idx)

### `set-zedit-config` : procedure/3

Usage: `(set-zedit-config zedit sel b)`

Set a configuration property of the given `zedit` to `b`. The selector `sel` can be one of the following: show-line-numbers? - show the line number display if true, show-whitespace? - show whitespace characters if true, line-wrap? - wrap lines to the size of the widget automatically, soft-wrap? - do not end lines in a hard line feed when line wrapping (default), draw-caret? - draw the caret if true, supress caret drawing if false, highlight-parens? - automatically highlight the matching opening paren while typing and moving the caret after a closing paren, highlight-paren-range? - automatically highlight the range of characters between the opening and closing paren when hightlight-parens? is true, get-word-at-left? - returns word left of caret if true, otherwise words are only triggered when the caret is on them, liberal-get-word-at? - words are parsed more like Z3S5 Lisp symbols, i.e., including all kinds of delimiters except for parentheses.

See also: [`new-zedit`](#link6e65772d7a65646974), [`get-zedit-config`](#link6765742d7a656469742d636f6e666967).	 [→index](#idx)

### set-zedit-event-handler : nil

Usage: `(set-zedit-event-handler editor sel proc)`

Set the event handler for `sel` and the given `event` to `proc`. The handler `proc` is a function that takes an event selector and an editor as argument. The selector `sel` must be one of '(caret-move word-change).

See also: [`new-zedit`](#link6e65772d7a65646974), [`remove-zedit-event-handler`](#link72656d6f76652d7a656469742d6576656e742d68616e646c6572).	 [→index](#idx)

### `set-zedit-line-number-style` : procedure/2

Usage: `(set-zedit-line-number-style zedit li)`

Set the style of the line number display in `zedit`, even when the line number display is off. The style list is for en editor style, an a-list containing boolean keys in '(bold italic monospace) followed by nil or t, and color keys 'text-color and 'background-color followed by a 16-bit (r g b a) color list.

See also: [`new-zedit`](#link6e65772d7a65646974), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65).	 [→index](#idx)

### `set-zedit-text` : procedure/2

Usage: `(set-zedit-text zedit str)`

Sets the text in the zedit editor to `str.`

See also: [`new-zedit`](#link6e65772d7a65646974), [`get-zedit-text`](#link6765742d7a656469742d74657874).	 [→index](#idx)

### `set-zedit-top-line` : procedure/2

Usage: `(set-zedit-top-line zedit n)`

Set the top visible line in `zedit` to `n`, scrolling the editor accordingly.

See also: [`get-zedit-top-line`](#link6765742d7a656469742d746f702d6c696e65).	 [→index](#idx)

### `show-color-picker` : procedure/3 or more

Usage: `(show-color-picker title message proc [win]`

Show a color picker dialog with given `title` and `message`. The callback `proc` takes a color list and is called when a color is chosen. The optional `win` argument specifies the parent window.

See also: [`nrgba`](#link6e72676261), [`nrgba64`](#link6e726762613634), [`color`](#link636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`theme-color`](#link7468656d652d636f6c6f72).	 [→index](#idx)

### `show-confirm` : procedure/4

Usage: `(show-confirm title message proc win)`

Show a simple confirmation dialog with Yes and No as options and `title` and `message`. The callback `proc` takes a bool that is true if the dialog has been confirmed, nil otherwise. The `win` argument specifies the parent window.

See also: [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d), [`show-custom`](#link73686f772d637573746f6d), [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73), [`show-information`](#link73686f772d696e666f726d6174696f6e), [`show-form`](#link73686f772d666f726d).	 [→index](#idx)

### `show-custom` : procedure/4

Usage: `(show-custom title dismiss content win)`

Show a custom info dialog, where `title` is the title of the dialog, `dismiss` is a string for the button text to dismiss the dialog, `content` is a valid canvas object ID for the content of the dialog such as the ID of an entry or form, and `win` is the parent window.

See also: [`show-confirm`](#link73686f772d636f6e6669726d), [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d), [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73), [`show-information`](#link73686f772d696e666f726d6174696f6e), [`show-form`](#link73686f772d666f726d).	 [→index](#idx)

### `show-custom-confirm` : procedure/6

Usage: `(show-custom-confirm title confirm dismiss content proc win)`

Show a custom confirm dialog, where `title` is the dialog's title, `confirm` is the text of the confirm button, `dismiss` is the text of the dismiss button, content is a valid canvas object ID for the content of the dialog such as an ID for an entry or form, `proc` is a callback procedure that takes one argument that is true if the dialog was confirmed, nil otherwise, and `win` is the parent window.

See also: [`show-confirm`](#link73686f772d636f6e6669726d), [`show-custom`](#link73686f772d637573746f6d), [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73), [`show-information`](#link73686f772d696e666f726d6174696f6e), [`show-form`](#link73686f772d666f726d).	 [→index](#idx)

### `show-custom-without-buttons` : procedure/3

Usage: `(show-custom-without-buttons title content win)`

Show a custom dialog without buttons, where `title` is the dialog's title, `content` is a valid canvas object ID for the content of the dialog such as an ID for an entry or form, and `win` is the parent window.

See also: [`show-custom`](#link73686f772d637573746f6d), [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d), [`show-confirm`](#link73686f772d636f6e6669726d), [`show-information`](#link73686f772d696e666f726d6174696f6e), [`show-form`](#link73686f772d666f726d).	 [→index](#idx)

### `show-file-open` : procedure/2

Usage: `(show-file-open proc win)`

Show a standard file open dialog that allows the user to select an existing file. The procedure `proc` takes two arguments. The first argument is a readable port if the user chose a file, nil if the user canceled. The second argument is nil if no error occurred, and an error string if an error occurred. Notice that the port can be not-nil and the error string can be non-empty at the same time, indicating that a file for opening was chosen but some I/O error occurred.

See also: [`show-file-open`](#link73686f772d66696c652d6f70656e).	 [→index](#idx)

### `show-file-save` : procedure/2

Usage: `(show-file-save proc win)`

Show a standard file save dialog that allows the user to chose a save file. If the chosen file already exists, the user is asked whether they want to overwrite the file. The procedure `proc` takes two arguments. The first argument is a writeable port if the user chose a file, nil if the user canceled. The second argument is nil if no error occurred, and an error string if an error occurred. Notice that the port can be not-nil and the error string can be non-empty at the same time, indicating that a file for saving was chosen but some I/O error occurred.

See also: [`show-file-open`](#link73686f772d66696c652d6f70656e).	 [→index](#idx)

### `show-folder-open` : procedure/2

Usage: `(show-folder-open proc win)`

Show an open folder dialog to select a folder. The procedure `proc` takes two arguments. The first argument is an URI string if a folder was chosen, nil otherwise. The second argument is nil if no error occurred, and an error string if an error occurred. Notice that URI can be not-nil and the error string can be non-empty at the same time, indicating that a folder was chosen but some I/O error occurred.

See also: [`list-uri`](#link6c6973742d757269), [`listable-uri?`](#link6c69737461626c652d7572693f), [`uri?`](#link7572693f).	 [→index](#idx)

### `show-form` : procedure/6

Usage: `(show-form title confirm dismiss li proc win)`

Show a list of form items whose ID must reside in `li` and check these items are validated. The `title` is the dialog's title, `confirm` is the text of the confirm button, `dismiss` the text of the dismiss button, `proc` is a procedure that takes a boolean argument that is true if all form items have been validated and confirmed, nil otherwise, and `win` is the parent window. The form items in `li` must be widgets with validators that can be added to a form.

See also: [`show-custom`](#link73686f772d637573746f6d), [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73), [`show-confirm`](#link73686f772d636f6e6669726d), [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d).	 [→index](#idx)

### `show-information` : procedure/3

Usage: `(show-information title message win)`

Show a dialog with `title` and information `message` strings, where `win` is the parent window.

See also: [`show-custom`](#link73686f772d637573746f6d), [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73), [`show-confirm`](#link73686f772d636f6e6669726d), [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d), [`show-form`](#link73686f772d666f726d).	 [→index](#idx)

### `show-object` : procedure/1

Usage: `(show-object obj)`

Show the canvas object `obj.`

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

### `text-grid-show-line-numbers?` : procedure/1

Usage: `(text-grid-show-line-numbers? grid) => bool`

Return true if the text `grid` shows line numbers, nil otherwise.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx)

### `text-grid-show-whitespace?` : procedure/1

Usage: `(text-grid-show-whitespace? grid) => bool`

Return true if the text `grid` shows whitespace glyphs, nil otherwise.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx)

### `theme-color` : procedure/1

Usage: `(theme-color selector) => li`

Obtain a theme color as color list. `selector` must be one of '(foreground background button disabled-button disabled disabled-text error focus hover input-background input-border menu-background overlay-background place-holder pressed primary scroll-bar selection separator shadow success warning).

See also: [`theme-icon`](#link7468656d652d69636f6e), [`nrgba64`](#link6e726762613634), [`nrgba`](#link6e72676261), [`color->color-64`](#link636f6c6f722d3e636f6c6f722d3634), [`color-64->color`](#link636f6c6f722d36342d3e636f6c6f72), [`*colors*`](#link2a636f6c6f72732a).	 [→index](#idx)

### `theme-icon` : procedure/1

Usage: `(theme-icon selector) => int`

Obtain a pre-defined icon from the application's theme based on the symbol `selector`, which may be one of '(cancel check-button check-button-checked color-achromatic color-chromatic color-palette computer confirm content-add content-clear content-copy content-cut content-paste content-redo content-remove content-undo delete document-create document-print document download error file-application file-audio file-image file-text file-video file folder-new folder-open folder grid help history home info list login logout mail-attachment mail-compose mail-forward mail-reply-all mail-reply mail-send media-fast-forward media-fast-rewind media-music media-pause media-photo media-play media-record media-replay media-skip-next media-skip-previous media-stop media-video media-expand menu more-horizontal more-vertical move-down move-up navigate-back navigate-next question radio-button radio-button-checked search-replace search settings storage upload view-full-screen view-refresh view-restore visibility-off visibility volume-down volume-mute volume-up warning).

See also: [`new-icon`](#link6e65772d69636f6e), [`new-image-from-`](#link6e65772d696d6167652d66726f6d2d), [`new-image-from-resource`](#link6e65772d696d6167652d66726f6d2d7265736f75726365).	 [→index](#idx)

### `theme-is-dark?` : procedure/1

Usage: `(theme-is-dark?) => bool`

Return true if the current GUI theme is dark, nil otherwise.

See also: [`theme-color`](#link7468656d652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a).	 [→index](#idx)

### `trigger-menu-last` : procedure/1

Usage: `(trigger-menu-last menu)`

Find the last active menu or submenu item and trigger it.

See also: [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75), [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874), [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573), [`new-menu`](#link6e65772d6d656e75).	 [→index](#idx)

### `unfocus-canvas-objects` : procedure/1

Usage: `(unfocus-canvas-objects canvas)`

Remove the focus on any user interface element in `canvas.`

See also: [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374), [`focus-next-canvas-object`](#link666f6375732d6e6578742d63616e7661732d6f626a656374), [`focus-previous-canvas-object`](#link666f6375732d70726576696f75732d63616e7661732d6f626a656374), [`get-focused-canvas-object`](#link6765742d666f63757365642d63616e7661732d6f626a656374).	 [→index](#idx)

### `uri?` : procedure/1

Usage: `(uri? s) => bool`

Return true if string `s` represents a valid URI, nil otherwise.

See also: [`list-uri`](#link6c6973742d757269), [`listable-uri?`](#link6c69737461626c652d7572693f).	 [→index](#idx)

### `validate-object` : procedure/2

Usage: `(validate-object obj) => str`

Validate the validatable object `obj` programmatically and return the validation failure as string, or the empty string if validation succeeded. It sometimes makes sense to call this explicitly in order to force the object to display its validation state.

See also: [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b), [`new-validator`](#link6e65772d76616c696461746f72), [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72).	 [→index](#idx)

### `window-fixed-size?` : procedure/1

Usage: `(window-fixed-size? window) => bool`

Return nil if `window` is fixed size, true otherwise.

See also: [`set-window-fixed-size`](#link7365742d77696e646f772d66697865642d73697a65).	 [→index](#idx)

### `window-full-screen?` : procedure/1

Usage: `(window-full-screen? window) => bool`

Return nil if `window` is full screen, true otherwise.

See also: [`set-window-full-screen`](#link7365742d77696e646f772d66756c6c2d73637265656e), [`center-window-on-screen`](#link63656e7465722d77696e646f772d6f6e2d73637265656e).	 [→index](#idx)

### `window-padded?` : procedure/1

Usage: `(window-padded? window) => bool`

Return nil if `window` is padded, true otherwise.

See also: [`set-window-padded`](#link7365742d77696e646f772d706164646564), [`set-window-full-screen`](#link7365742d77696e646f772d66756c6c2d73637265656e), [`center-window-on-screen`](#link63656e7465722d77696e646f772d6f6e2d73637265656e).	 [→index](#idx)

### `wrap-delete-text-grid` : procedure/8

Usage: `(wrap-delete-text-grid grid range-list wrapcol soft-wrap? hard-lf-rune soft-lf-rune cursor-row cursor-column) => li`

This helper implements deletion with word wrapping in `grid`. The `range-list` must contain integers of the form (start-row start-colum end-row end-column), which must be within the grid's maximum row and column ranges. `wrapcol` is an integer indicating the number of chars per line; any more chars are wrapped. If `soft-wrap?` is not nil, then the paragraphs in which deletion takes place are soft-wrapped. `hard-lf-rune` is a string containing the rune for a hard line feed, whereas `soft-lf-rune` is a string containing the rune for soft line feeds. The current `cursor-row` and `cursor-column` must be provided as well; when the function wraps the deleted paragraphs, their values are updated and returned in the list `li`, which is of the form (new-cursor-row new-cursor-column).

See also: [`wrap-insert-text-grid`](#link777261702d696e736572742d746578742d67726964), [`new-text-grid`](#link6e65772d746578742d67726964).	 [→index](#idx)

### `wrap-insert-text-grid` : procedure/8

Usage: `(wrap-insert-text-grid grid cells row col wrapcol soft-wrap? hard-lf-rune soft-lf-rune) => li`

This helper implements inserting styled text with word wrapping in `grid`. `cells` must be a list of text grid cells, each of which consists of a rune string, and a list containing a foreground and background color, or nil. `row` and `col` are the line and column in `grid` before which the text is inserted. The number of characters per line is indicated with `wrapcol`. If `soft-wrap?` is true, then the paragraph into which it is inserted is soft-word-wrapped, using soft-lf-rune as a line ending. Otherwise, `hard-lf-rune` is used for line-endings, which is also used for the last line of a paragraph. The returned list of the form (new-cursor-row new-cursor-column) reflects the updated cursor position if `row` and `col` are the current cursor position.

See also: [`wrap-delete-text-grid`](#link777261702d64656c6574652d746578742d67726964), [`new-text-grid`](#link6e65772d746578742d67726964).	 [→index](#idx)

### `zedit-current-selection-text` : procedure/1

Usage: `(zedit-current-selection-text editor) => str`

Obtain the currently selected text in `editor`, the empty string if no text is selected.

See also: [`get-zedit-text`](#link6765742d7a656469742d74657874), [`get-zedit-text-range`](#link6765742d7a656469742d746578742d72616e6765), [`get-zedit-selection`](#link6765742d7a656469742d73656c656374696f6e).	 [→index](#idx)



## Help System {#help}

This section lists functions related to the built-in help system.

### *help* : dict

Usage: `*help*`

Dict containing all help information for symbols.

See also: [`help`](#link68656c70), [`defhelp`](#link64656668656c70), [`apropos`](#link6170726f706f73).	 [→index](#idx)

### `apropos` : procedure/1

Usage: `(apropos sym) => #li`

Get a list of procedures and symbols related to `sym` from the help system.

See also: [`defhelp`](#link64656668656c70), [`help-entry`](#link68656c702d656e747279), [`help`](#link68656c70), [`*help*`](#link2a68656c702a).	 [→index](#idx)

### `help` : macro/1

Usage: `(help sym)`

Display help information about `sym` (unquoted).

See also: [`defhelp`](#link64656668656c70), [`help-topics`](#link68656c702d746f70696373), [`help-about`](#link68656c702d61626f7574), [`help-topic-info`](#link68656c702d746f7069632d696e666f), [`set-help-topic-info`](#link7365742d68656c702d746f7069632d696e666f), [`help-entry`](#link68656c702d656e747279), [`*help*`](#link2a68656c702a), [`apropos`](#link6170726f706f73).	 [→index](#idx)

### help->manual-entry : nil

Usage: `(help->manual-entry key [level] [link?]) => str`

Looks up help for `key` and converts it to a manual section as markdown string. If there is no entry for `key`, then nil is returned. The optional `level` integer indicates the heading nesting. If `link?` is true an anchor is created for the key.

See also: [`help`](#link68656c70).	 [→index](#idx)

### `help-about` : procedure/1 or more

Usage: `(help-about topic [sel]) => li`

Obtain a list of symbols for which help about `topic` is available. If optional `sel` argument is left out or `any`, then any symbols with which the topic is associated are listed. If the optional `sel` argument is `first`, then a symbol is only listed if it has `topic` as first topic entry. This restricts the number of entries returned to a more essential selection.

See also: [`help-topics`](#link68656c702d746f70696373), [`help`](#link68656c70), [`apropos`](#link6170726f706f73).	 [→index](#idx)

### `help-entry` : procedure/1

Usage: `(help-entry sym) => list`

Get usage and help information for `sym.`

See also: [`defhelp`](#link64656668656c70), [`help`](#link68656c70), [`apropos`](#link6170726f706f73), [`*help*`](#link2a68656c702a), [`help-topics`](#link68656c702d746f70696373), [`help-about`](#link68656c702d61626f7574), [`set-help-topic-info`](#link7365742d68656c702d746f7069632d696e666f), [`help-topic-info`](#link68656c702d746f7069632d696e666f).	 [→index](#idx)

### `help-strings` : procedure/2

Usage: `(help-strings sym del) => li`

Obtain a string of help strings for a given symbol `sym`. The fields in the string are separated by string `del.`

See also: [`help`](#link68656c70), [`help-entry`](#link68656c702d656e747279), [`*help*`](#link2a68656c702a).	 [→index](#idx)

### `help-topic-info` : procedure/1

Usage: `(help-topic-info topic) => li`

Return a list containing a heading and an info string for help `topic`, or nil if no info is available.

See also: [`set-help-topic-info`](#link7365742d68656c702d746f7069632d696e666f), [`defhelp`](#link64656668656c70), [`help`](#link68656c70).	 [→index](#idx)

### `help-topics` : procedure/0

Usage: `(help-topics) => li`

Obtain a list of help topics for commands.

See also: [`help`](#link68656c70), [`help-topic`](#link68656c702d746f706963), [`apropos`](#link6170726f706f73).	 [→index](#idx)

### `prune-unneeded-help-entries` : procedure/0

Usage: `(prune-unneeded-help-entries)`

Remove help entries for which no toplevel symbol is defined. This function may need to be called when a module is not being used (e.g. because of a missing build tag) and it is desirable that only help for existing symbols is available.

See also: [`find-unneeded-help-entries`](#link66696e642d756e6e65656465642d68656c702d656e7472696573), [`find-missing-help-entries`](#link66696e642d6d697373696e672d68656c702d656e7472696573), [`help`](#link68656c70), [`*help*`](#link2a68656c702a).	 [→index](#idx)

### `set-help-topic-info` : procedure/3

Usage: `(set-help-topic-info topic header info)`

Set a human-readable information entry for help `topic` with human-readable `header` and `info` strings.

See also: [`defhelp`](#link64656668656c70), [`help-topic-info`](#link68656c702d746f7069632d696e666f).	 [→index](#idx)















## Library System {#lib}

This miscellaneous mini-library system allows importing programs with a prefix by source-transforming them.

### `global-sym?` : procedure/1

Usage: `(global-sym? sym) => bool`

Returns true if `sym` is a global symbol, nil otherwise. By convention, a symbol counts as global if it starts with a "*" character. This is used by library functions to determine whether a top-level symbol ought to be treated as local or global to the library.

See also: [`load`](#link6c6f6164), [`include`](#link696e636c756465), [`sym?`](#link73796d3f).	 [→index](#idx)

### `load` : procedure/1 or more

Usage: `(load prefix [fi])`

Loads the Lisp file at `fi` as a library or program with the given `prefix`. If only a prefix is specified, load attempts to find a corresponding file at path (str+ (sysdir 'z3s5-data) "/prg/prefix/prefix.lisp"). Loading binds all non-global toplevel symbols of the definitions in file `fi` to the form prefix.symbol and replaces calls to them in the definitions appropriately. Symbols starting with "*" such as *cancel* are not modified. To give an example, if `fi` contains a definition (defun bar ...) and the prefix is 'foo, then the result of the import is equivalent to (defun foo.bar ...), and so on for any other definitions. The importer preorder-traverses the source and looks for setq and lambdas after macro expansion has taken place. By convention, the entry point of executable programs is a function (run) so the loaded program can be executed with the command (prefix.run).

See also: [`include`](#link696e636c756465), [`global-sym?`](#link676c6f62616c2d73796d3f).	 [→index](#idx)



## Soundex, Metaphone, etc. {#ling}

The package `ling` provides various phonemic transcription functions like Soundex and Metaphone that are commonly used for fuzzy search and similarity comparisons between strings.

### `ling.damerau-levenshtein` : procedure/2

Usage: `(ling.damerau-levenshtein s1 s2) => num`

Compute the Damerau-Levenshtein distance between `s1` and `s2.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)

### `ling.hamming` : procedure/2

Usage: `(ling-hamming s1 s2) => num`

Compute the Hamming distance between `s1` and `s2.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)

### `ling.jaro` : procedure/2

Usage: `(ling.jaro s1 s2) => num`

Compute the Jaro distance between `s1` and `s2.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)

### `ling.jaro-winkler` : procedure/2

Usage: `(ling.jaro-winkler s1 s2) => num`

Compute the Jaro-Winkler distance between `s1` and `s2.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)

### `ling.levenshtein` : procedure/2

Usage: `(ling.levenshtein s1 s2) => num`

Compute the Levenshtein distance between `s1` and `s2.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)

### `ling.match-rating-codex` : procedure/1

Usage: `(ling.match-rating-codex s) => str`

Compute the Match-Rating-Codex of string `s.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)

### `ling.match-rating-compare` : procedure/2

Usage: `(ling.match-rating-compare s1 s2) => bool`

Returns true if `s1` and `s2` are equal according to the Match-rating Comparison algorithm, nil otherwise.

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)

### `ling.metaphone` : procedure/1

Usage: `(ling.metaphone s) => str`

Compute the Metaphone representation of string `s.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)

### `ling.nysiis` : procedure/1

Usage: `(ling.nysiis s) => str`

Compute the Nysiis representation of string `s.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)

### `ling.porter` : procedure/1

Usage: `(ling.porter s) => str`

Compute the stem of word string `s` using the Porter stemming algorithm.

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)

### `ling.soundex` : procedure/1

Usage: `(ling.soundex s) => str`

Compute the Soundex representation of string `s.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx)



## Lisp - Traditional Lisp Functions {#lisp}

This section comprises a large number of list processing functions as well the standard control flow macros and functions you'd expect in a Lisp system.

### `alist?` : procedure/1

Usage: `(alist? li) => bool`

Return true if `li` is an association list, nil otherwise. This also works for a-lists where each element is a pair rather than a full list.

See also: [`assoc`](#link6173736f63).	 [→index](#idx)

### `and` : macro/0 or more

Usage: `(and expr1 expr2 ...) => any`

Evaluate `expr1` and if it is not nil, then evaluate `expr2` and if it is not nil, evaluate the next expression, until all expressions have been evaluated. This is a shortcut logical and.

See also: [`or`](#link6f72).	 [→index](#idx)

### `append` : procedure/1 or more

Usage: `(append li1 li2 ...) => li`

Concatenate the lists given as arguments.

See also: [`cons`](#link636f6e73).	 [→index](#idx)

### `apply` : procedure/2

Usage: `(apply proc arg) => any`

Apply function `proc` to argument list `arg.`

See also: [`functional?`](#link66756e6374696f6e616c3f).	 [→index](#idx)

### `assoc` : procedure/2

Usage: `(assoc key alist) => li`

Return the sublist of `alist` that starts with `key` if there is any, nil otherwise. Testing is done with equal?. An association list may be of the form ((key1 value1)(key2 value2)...) or ((key1 . value1) (key2 . value2) ...)

See also: [`assoc`](#link6173736f63), [`assoc1`](#link6173736f6331), [`alist?`](#link616c6973743f), [`eq?`](#link65713f), [`equal?`](#link657175616c3f).	 [→index](#idx)

### `assoc1` : procedure/2

Usage: `(assoc1 sym li) => any`

Get the second element in the first sublist in `li` that starts with `sym`. This is equivalent to (cadr (assoc sym li)).

See also: [`assoc`](#link6173736f63), [`alist?`](#link616c6973743f).	 [→index](#idx)

### `assq` : procedure/2

Usage: `(assq key alist) => li`

Return the sublist of `alist` that starts with `key` if there is any, nil otherwise. Testing is done with eq?. An association list may be of the form ((key1 value1)(key2 value2)...) or ((key1 . value1) (key2 . value2) ...)

See also: [`assoc`](#link6173736f63), [`assoc1`](#link6173736f6331), [`eq?`](#link65713f), [`alist?`](#link616c6973743f), [`equal?`](#link657175616c3f).	 [→index](#idx)

### `atom?` : procedure/1

Usage: `(atom? x) => bool`

Return true if `x` is an atomic value, nil otherwise. Atomic values are numbers and symbols.

See also: [`sym?`](#link73796d3f).	 [→index](#idx)

### `bool?` : procedure/1

Usage: `(bool? datum) => bool`

Return true if `datum` is either true or nil. Note: This predicate only exists for type-completeness and you should never use it as part of testing whether something is true or false - per convention, a value is true if it is non-nil and not when it is true, which is the special boolean value this predicate tests in addition to nil.

See also: [`null?`](#link6e756c6c3f), [`not`](#link6e6f74).	 [→index](#idx)

### `build-list` : procedure/2

Usage: `(build-list n proc) => list`

Build a list with `n` elements by applying `proc` to the counter `n` each time.

See also: [`list`](#link6c697374), [`list?`](#link6c6973743f), [`map`](#link6d6170), [`foreach`](#link666f7265616368).	 [→index](#idx)

### `caaar` : procedure/1

Usage: `(caaar x) => any`

Equivalent to (car (car (car `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `caadr` : procedure/1

Usage: `(caadr x) => any`

Equivalent to (car (car (cdr `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `caar` : procedure/1

Usage: `(caar x) => any`

Equivalent to (car (car `x`)).

See also: [`car`](#link636172), [`cdr`](#link636472), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `cadar` : procedure/1

Usage: `(cadar x) => any`

Equivalent to (car (cdr (car `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `caddr` : procedure/1

Usage: `(caddr x) => any`

Equivalent to (car (cdr (cdr `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `cadr` : procedure/1

Usage: `(cadr x) => any`

Equivalent to (car (cdr `x`)).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `car` : procedure/1

Usage: `(car li) => any`

Get the first element of a list or pair `li`, an error if there is not first element.

See also: [`list`](#link6c697374), [`list?`](#link6c6973743f), [`pair?`](#link706169723f).	 [→index](#idx)

### `case` : macro/2 or more

Usage: `(case expr (clause1 ... clausen)) => any`

Standard case macro, where you should use t for the remaining alternative. Example: (case (get dict 'key) ((a b) (out "a or b"))(t (out "something else!"))).

See also: [`cond`](#link636f6e64).	 [→index](#idx)

### `cdaar` : procedure/1

Usage: `(cdaar x) => any`

Equivalent to (cdr (car (car `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `cdadr` : procedure/1

Usage: `(cdadr x) => any`

Equivalent to (cdr (car (cdr `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `cdar` : procedure/1

Usage: `(cdar x) => any`

Equivalent to (cdr (car `x`)).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `cddar` : procedure/1

Usage: `(cddar x) => any`

Equivalent to (cdr (cdr (car `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `cdddr` : procedure/1

Usage: `(cdddr x) => any`

Equivalent to (cdr (cdr (cdr `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `cddr` : procedure/1

Usage: `(cddr x) => any`

Equivalent to (cdr (cdr `x`)).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx)

### `cdr` : procedure/1

Usage: `(cdr li) => any`

Get the rest of a list `li`. If the list is proper, the cdr is a list. If it is a pair, then it may be an element. If the list is empty, nil is returned.

See also: [`car`](#link636172), [`list`](#link6c697374), [`list?`](#link6c6973743f), [`pair?`](#link706169723f).	 [→index](#idx)

### cond : special form

Usage: `(cond ((test1 expr1 ...) (test2 expr2 ...) ...) => any`

Evaluate the tests sequentially and execute the expressions after the test when a test is true. To express the else case, use (t exprn ...) at the end of the cond-clauses to execute `exprn`...

See also: [`if`](#link6966), [`when`](#link7768656e), [`unless`](#link756e6c657373).	 [→index](#idx)

### `cons` : procedure/2

Usage: `(cons a b) => pair`

Cons two values into a pair. If `b` is a list, the result is a list. Otherwise the result is a pair.

See also: [`cdr`](#link636472), [`car`](#link636172), [`list?`](#link6c6973743f), [`pair?`](#link706169723f).	 [→index](#idx)

### `cons?` : procedure/1

Usage: `(cons? x) => bool`

return true if `x` is not an atom, nil otherwise.

See also: [`atom?`](#link61746f6d3f).	 [→index](#idx)

### `count-partitions` : procedure/2

Usage: `(count-partitions m k) => int`

Return the number of partitions for divding `m` items into parts of size `k` or less, where the size of the last partition may be less than `k` but the remaining ones have size `k.`

See also: [`nth-partition`](#link6e74682d706172746974696f6e), [`get-partitions`](#link6765742d706172746974696f6e73).	 [→index](#idx)

### `defmacro` : macro/2 or more

Usage: `(defmacro name args body ...)`

Define a macro `name` with argument list `args` and `body`. Macros are expanded at compile-time.

See also: [`macro`](#link6d6163726f).	 [→index](#idx)

### `defun` : macro/1 or more

Usage: `(defun ident (params ...) body ...)`

Define a function with name `ident`, a possibly empty list of `params`, and the remaining `body` expressions. This is a macro for (setq ident (lambda (params ...) body ...)) and binds the lambda-form to the given symbol. Like lambdas, the `params` of `defun` allow for a `&rest` keyword before the last parameter name. This binds all remaining arguments of a variadic function call to this parameter as a list.

See also: [`setq`](#link73657471), [`defmacro`](#link6465666d6163726f).	 [→index](#idx)

### `dolist` : macro/1 or more

Usage: `(dolist (name list [result]) body ...) => li`

Traverse the list `list` in order, binding `name` to each element subsequently and evaluate the `body` expressions with this binding. The optional `result` is the result of the traversal, nil if it is not provided.

See also: [`letrec`](#link6c6574726563), [`foreach`](#link666f7265616368), [`map`](#link6d6170).	 [→index](#idx)

### `dotimes` : macro/1 or more

Usage: `(dotimes (name count [result]) body ...) => any`

Iterate `count` times, binding `name` to the counter starting from 0 until the counter has reached count-1, and evaluate the `body` expressions each time with this binding. The optional `result` is the result of the iteration, nil if it is not provided.

See also: [`letrec`](#link6c6574726563), [`dolist`](#link646f6c697374), [`while`](#link7768696c65).	 [→index](#idx)

### `eq?` : procedure/2

Usage: `(eq? x y) => bool`

Return true if `x` and `y` are equal, nil otherwise. In contrast to other LISPs, eq? checks for deep equality of arrays and dicts. However, lists are compared by checking whether they are the same cell in memory. Use `equal?` to check for deep equality of lists and other objects.

See also: [`equal?`](#link657175616c3f).	 [→index](#idx)

### `eql?` : procedure/2

Usage: `(eql? x y) => bool`

Returns true if `x` is equal to `y`, nil otherwise. This is currently the same as equal? but the behavior might change.

See also: [`equal?`](#link657175616c3f).	 [→index](#idx)

**Warning: Deprecated.**

### `equal?` : procedure/2

Usage: `(equal? x y) => bool`

Return true if `x` and `y` are equal, nil otherwise. The equality is tested recursively for containers like lists and arrays.

See also: [`eq?`](#link65713f), [`eql?`](#link65716c3f).	 [→index](#idx)

### `filter` : procedure/2

Usage: `(filter li pred) => li`

Return the list based on `li` with each element removed for which `pred` returns nil.

See also: [`list`](#link6c697374).	 [→index](#idx)

### `flatten` : procedure/1

Usage: `(flatten lst) => list`

Flatten `lst`, making all elements of sublists elements of the flattened list.

See also: [`car`](#link636172), [`cdr`](#link636472), [`remove-duplicates`](#link72656d6f76652d6475706c696361746573).	 [→index](#idx)

### `get-partitions` : procedure/2

Usage: `(get-partitions x n) => proc/1*`

Return an iterator procedure that returns lists of the form (start-offset end-offset bytes) with 0-index offsets for a given index `k`, or nil if there is no corresponding part, such that the sizes of the partitions returned in `bytes` summed up are `x` and and each partition is `n` or lower in size. The last partition will be the smallest partition with a `bytes` value smaller than `n` if `x` is not dividable without rest by `n`. If no argument is provided for the returned iterator, then it returns the number of partitions.

See also: [`nth-partition`](#link6e74682d706172746974696f6e), [`count-partitions`](#link636f756e742d706172746974696f6e73), [`get-file-partitions`](#link6765742d66696c652d706172746974696f6e73), [`iterate`](#link69746572617465).	 [→index](#idx)

### `identity` : procedure/1

Usage: `(identity x)`

Return `x.`

See also: [`apply`](#link6170706c79), [`equal?`](#link657175616c3f).	 [→index](#idx)

### `if` : macro/3

Usage: `(if cond expr1 expr2) => any`

Evaluate `expr1` if `cond` is true, otherwise evaluate `expr2.`

See also: [`cond`](#link636f6e64), [`when`](#link7768656e), [`unless`](#link756e6c657373).	 [→index](#idx)

### `iterate` : procedure/2

Usage: `(iterate it proc)`

Apply `proc` to each argument returned by iterator `it` in sequence, similar to the way foreach works. An iterator is a procedure that takes one integer as argument or no argument at all. If no argument is provided, the iterator returns the number of iterations. If an integer is provided, the iterator returns a non-nil value for the given index.

See also: [`foreach`](#link666f7265616368), [`get-partitions`](#link6765742d706172746974696f6e73).	 [→index](#idx)

### lambda : special form

Usage: `(lambda args body ...) => closure`

Form a function closure (lambda term) with argument list in `args` and body expressions `body.`

See also: [`defun`](#link646566756e), [`functional?`](#link66756e6374696f6e616c3f), [`macro?`](#link6d6163726f3f), [`closure?`](#link636c6f737572653f).	 [→index](#idx)

### `lcons` : procedure/2

Usage: `(lcons datum li) => list`

Insert `datum` at the end of the list `li`. There may be a more efficient implementation of this in the future. Or, maybe not. Who knows?

See also: [`cons`](#link636f6e73), [`list`](#link6c697374), [`append`](#link617070656e64), [`nreverse`](#link6e72657665727365).	 [→index](#idx)

### `let` : macro/1 or more

Usage: `(let args body ...) => any`

Bind each pair of symbol and expression in `args` and evaluate the expressions in `body` with these local bindings. Return the value of the last expression in `body.`

See also: [`letrec`](#link6c6574726563).	 [→index](#idx)

### `letrec` : macro/1 or more

Usage: `(letrec args body ...) => any`

Recursive let binds the symbol, expression pairs in `args` in a way that makes prior bindings available to later bindings and allows for recursive definitions in `args`, then evaluates the `body` expressions with these bindings.

See also: [`let`](#link6c6574).	 [→index](#idx)

### `list` : procedure/0 or more

Usage: `(list [args] ...) => li`

Create a list from all `args`. The arguments must be quoted.

See also: [`cons`](#link636f6e73).	 [→index](#idx)

### `list-exists?` : procedure/2

Usage: `(list-exists? li pred) => bool`

Return true if `pred` returns true for at least one element in list `li`, nil otherwise.

See also: [`exists?`](#link6578697374733f), [`forall?`](#link666f72616c6c3f), [`array-exists?`](#link61727261792d6578697374733f), [`str-exists?`](#link7374722d6578697374733f), [`seq?`](#link7365713f).	 [→index](#idx)

### `list-forall?` : procedure/2

Usage: `(list-all? li pred) => bool`

Return true if predicate `pred` returns true for all elements of list `li`, nil otherwise.

See also: [`foreach`](#link666f7265616368), [`map`](#link6d6170), [`forall?`](#link666f72616c6c3f), [`array-forall?`](#link61727261792d666f72616c6c3f), [`str-forall?`](#link7374722d666f72616c6c3f), [`exists?`](#link6578697374733f).	 [→index](#idx)

### `list-foreach` : procedure/2

Usage: `(list-foreach li proc)`

Apply `proc` to each element of list `li` in order, for the side effects.

See also: [`mapcar`](#link6d6170636172), [`map`](#link6d6170), [`foreach`](#link666f7265616368).	 [→index](#idx)

### `list-last` : procedure/1

Usage: `(list-last li) => any`

Return the last element of `li.`

See also: [`reverse`](#link72657665727365), [`nreverse`](#link6e72657665727365), [`car`](#link636172), [`1st`](#link317374), [`last`](#link6c617374).	 [→index](#idx)

### `list-ref` : procedure/2

Usage: `(list-ref li n) => any`

Return the element with index `n` of list `li`. Lists are 0-indexed.

See also: [`array-ref`](#link61727261792d726566), [`nth`](#link6e7468).	 [→index](#idx)

### `list-reverse` : procedure/1

Usage: `(list-reverse li) => li`

Create a reversed copy of `li.`

See also: [`reverse`](#link72657665727365), [`array-reverse`](#link61727261792d72657665727365), [`str-reverse`](#link7374722d72657665727365).	 [→index](#idx)

### `list-slice` : procedure/3

Usage: `(list-slice li low high) => li`

Return the slice of the list `li` starting at index `low` (inclusive) and ending at index `high` (exclusive).

See also: [`slice`](#link736c696365), [`array-slice`](#link61727261792d736c696365).	 [→index](#idx)

### `list?` : procedure/1

Usage: `(list? obj) => bool`

Return true if `obj` is a list, nil otherwise.

See also: [`cons?`](#link636f6e733f), [`atom?`](#link61746f6d3f), [`null?`](#link6e756c6c3f).	 [→index](#idx)

### macro : special form

Usage: `(macro args body ...) => macro`

Like a lambda term but the `body` expressions are macro-expanded at compile time instead of runtime.

See also: [`defun`](#link646566756e), [`lambda`](#link6c616d626461), [`funcional?`](#link66756e63696f6e616c3f), [`macro?`](#link6d6163726f3f), [`closure?`](#link636c6f737572653f).	 [→index](#idx)

### `mapcar` : procedure/2

Usage: `(mapcar li proc) => li`

Return the list obtained from applying `proc` to each elements in `li.`

See also: [`map`](#link6d6170), [`foreach`](#link666f7265616368).	 [→index](#idx)

### `member` : procedure/2

Usage: `(member key li) => li`

Return the cdr of `li` starting with `key` if `li` contains an element equal? to `key`, nil otherwise.

See also: [`assoc`](#link6173736f63), [`equal?`](#link657175616c3f).	 [→index](#idx)

### `memq` : procedure/2

Usage: `(memq key li)`

Return the cdr of `li` starting with `key` if `li` contains an element eq? to `key`, nil otherwise.

See also: [`member`](#link6d656d626572), [`eq?`](#link65713f).	 [→index](#idx)

### `nconc` : procedure/0 or more

Usage: `(nconc li1 li2 ...) => li`

Concatenate `li1`, `li2`, and so forth, like with append, but destructively modifies `li1.`

See also: [`append`](#link617070656e64).	 [→index](#idx)

### `not` : procedure/1

Usage: `(not x) => bool`

Return true if `x` is nil, nil otherwise.

See also: [`and`](#link616e64), [`or`](#link6f72).	 [→index](#idx)

### `nreverse` : procedure/1

Usage: `(nreverse li) => li`

Destructively reverse `li.`

See also: [`reverse`](#link72657665727365).	 [→index](#idx)

### `nth-partition` : procedure/3

Usage: `(nth-partition m k idx) => li`

Return a list of the form (start-offset end-offset bytes) for the partition with index `idx` of `m` into parts of size `k`. The index `idx` as well as the start- and end-offsets are 0-based.

See also: [`count-partitions`](#link636f756e742d706172746974696f6e73), [`get-partitions`](#link6765742d706172746974696f6e73).	 [→index](#idx)

### `null?` : procedure/1

Usage: `(null? li) => bool`

Return true if `li` is nil, nil otherwise.

See also: [`not`](#link6e6f74), [`list?`](#link6c6973743f), [`cons?`](#link636f6e733f).	 [→index](#idx)

### `num?` : procedure/1

Usage: `(num? n) => bool`

Return true if `n` is a number (exact or inexact), nil otherwise.

See also: [`str?`](#link7374723f), [`atom?`](#link61746f6d3f), [`sym?`](#link73796d3f), [`closure?`](#link636c6f737572653f), [`intrinsic?`](#link696e7472696e7369633f), [`macro?`](#link6d6163726f3f).	 [→index](#idx)

### `or` : macro/0 or more

Usage: `(or expr1 expr2 ...) => any`

Evaluate the expressions until one of them is not nil. This is a logical shortcut or.

See also: [`and`](#link616e64).	 [→index](#idx)

### progn : special form

Usage: `(progn expr1 expr2 ...) => any`

Sequentially execute the expressions `expr1`, `expr2`, and so forth, and return the value of the last expression.

See also: [`defun`](#link646566756e), [`lambda`](#link6c616d626461), [`cond`](#link636f6e64).	 [→index](#idx)

### quasiquote : special form

Usage: `(quasiquote li)`

Quote `li`, except that values in `li` may be unquoted (~evaluated) when prefixed with "," and embedded lists can be unquote-spliced by prefixing them with unquote-splice ",@". An unquoted expression's value is inserted directly, whereas unquote-splice inserts the values of a list in-sequence into the embedding list. Quasiquote is used in combination with gensym to define non-hygienic macros. In Z3S5 Lisp, "," and ",@" are syntactic markers and there are no corresponding unquote and unquote-splice functions. The shortcut for quasiquote is "`".

See also: [`quote`](#link71756f7465), [`gensym`](#link67656e73796d), [`macro`](#link6d6163726f), [`defmacro`](#link6465666d6163726f).	 [→index](#idx)

### quote : special form

Usage: `(quote x)`

Quote symbol `x`, so it evaluates to `x` instead of the value bound to it. Syntactic shortcut is '.

See also: [`quasiquote`](#link717561736971756f7465).	 [→index](#idx)

### `replacd` : procedure/2

Usage: `(rplacd li1 li2) => li`

Destructively replace the cdr of `li1` with `li2` and return the result afterwards.

See also: [`rplaca`](#link72706c616361).	 [→index](#idx)

### `rplaca` : procedure/2

Usage: `(rplaca li a) => li`

Destructively mutate `li` such that its car is `a`, return the list afterwards.

See also: [`rplacd`](#link72706c616364).	 [→index](#idx)

### `setcar` : procedure/2

Usage: `(setcar li elem) => li`

Mutate `li` such that its car is `elem`. Same as rplaca.

See also: [`rplaca`](#link72706c616361), [`rplacd`](#link72706c616364), [`setcdr`](#link736574636472).	 [→index](#idx)

### `setcdr` : procedure/1

Usage: `(setcdr li1 li2) => li`

Mutate `li1` such that its cdr is `li2`. Same as rplacd.

See also: [`rplacd`](#link72706c616364), [`rplaca`](#link72706c616361), [`setcar`](#link736574636172).	 [→index](#idx)

### setq : special form

Usage: `(setq sym1 value1 ...)`

Set `sym1` (without need for quoting it) to `value`, and so forth for any further symbol, value pairs.

See also: [`bind`](#link62696e64), [`unbind`](#link756e62696e64).	 [→index](#idx)

### `sort` : procedure/2

Usage: `(sort li proc) => li`

Sort the list `li` by the given less-than procedure `proc`, which takes two arguments and returns true if the first one is less than the second, nil otheriwse.

See also: [`array-sort`](#link61727261792d736f7274).	 [→index](#idx)

### sort-symbols : nil

Usage: `(sort-symbols li) => list`

Sort the list of symbols `li` alphabetically.

See also: [`out`](#link6f7574), [`dp`](#link6470), [`du`](#link6475), [`dump`](#link64756d70).	 [→index](#idx)

### `sym?` : procedure/1

Usage: `(sym? sym) => bool`

Return true if `sym` is a symbol, nil otherwise.

See also: [`str?`](#link7374723f), [`atom?`](#link61746f6d3f).	 [→index](#idx)

### `type-of` : macro/1

Usage: `(type-of datum) => sym`

Returns the type of `datum` as symbol like type-of* but without having to quote the argument. If `datum` is an unbound symbol, then this macro returns 'unbound. Otherwise the type of a given symbol's value or the type of a given literal is returned.

See also: [`type-of*`](#link747970652d6f662a).	 [→index](#idx)

### `type-of*` : procedure/1

Usage: `(type-of* datum) => sym`

Return the type of `datum` as a symbol. This uses existing predicates and therefore is not faster than testing with predicates directly.

See also: [`num?`](#link6e756d3f), [`str?`](#link7374723f), [`sym?`](#link73796d3f), [`list?`](#link6c6973743f), [`array?`](#link61727261793f), [`bool?`](#link626f6f6c3f), [`eof?`](#link656f663f), [`boxed?`](#link626f7865643f), [`intrinsic?`](#link696e7472696e7369633f), [`closure?`](#link636c6f737572653f), [`macro?`](#link6d6163726f3f), [`blob?`](#link626c6f623f).	 [→index](#idx)

### `unless` : macro/1 or more

Usage: `(unless cond expr ...) => any`

Evaluate expressions `expr` if `cond` is not true, returns void otherwise.

See also: [`if`](#link6966), [`when`](#link7768656e), [`cond`](#link636f6e64).	 [→index](#idx)

### `until` : macro/1 or more

Usage: `(until test body ...)`

Evaluate the expressions in `body` until `test` is nil. Unlike a while loop, until executes the body expressions at least once.

See also: [`while`](#link7768696c65), [`letrec`](#link6c6574726563), [`dotimes`](#link646f74696d6573), [`dolist`](#link646f6c697374).	 [→index](#idx)

### `void` : procedure/0 or more

Usage: `(void [any] ...)`

Always returns void, no matter what values are given to it. Void is a special value that is not printed in the console.

See also: [`void?`](#link766f69643f).	 [→index](#idx)

### `void?` : procedure/1

Usage: `(void? datum)`

Return true if `datum` is the special symbol void, nil otherwise.

See also: [`void`](#link766f6964).	 [→index](#idx)

### `when` : macro/1 or more

Usage: `(when cond expr ...) => any`

Evaluate the expressions `expr` if `cond` is true, returns void otherwise.

See also: [`if`](#link6966), [`cond`](#link636f6e64), [`unless`](#link756e6c657373).	 [→index](#idx)

### `while` : macro/1 or more

Usage: `(while test body ...)`

Evaluate the expressions in `body` while `test` is not nil.

See also: [`until`](#link756e74696c), [`letrec`](#link6c6574726563), [`dotimes`](#link646f74696d6573), [`dolist`](#link646f6c697374).	 [→index](#idx)











## Numeric Functions {#numeric}

This section describes functions that provide standard arithmetics for non-floating point numbers such as integers. Notice that Z3S5 Lisp uses automatic bignum support but only for select standard operations like multiplication, addition, and subtraction.

### `%` : procedure/2

Usage: `(% x y) => num`

Compute the remainder of dividing number `x` by `y.`

See also: [`mod`](#link6d6f64), [`/`](#link2f).	 [→index](#idx)

### `*` : procedure/0 or more

Usage: `(* [args] ...) => num`

Multiply all `args`. Special cases: (*) is 1 and (* x) is x.

See also: [`+`](#link2b), [`-`](#link2d), [`/`](#link2f).	 [→index](#idx)

### `+` : procedure/0 or more

Usage: `(+ [args] ...) => num`

Sum up all `args`. Special cases: (+) is 0 and (+ x) is x.

See also: [`-`](#link2d), [`*`](#link2a), [`/`](#link2f).	 [→index](#idx)

### `-` : procedure/1 or more

Usage: `(- x [y1] [y2] ...) => num`

Subtract `y1`, `y2`, ..., from `x`. Special case: (- x) is -x.

See also: [`+`](#link2b), [`*`](#link2a), [`/`](#link2f).	 [→index](#idx)

### `/` : procedure/1 or more

Usage: `(/ x y1 [y2] ...) => float`

Divide `x` by `y1`, then by `y2`, and so forth. The result is a float.

See also: [`+`](#link2b), [`*`](#link2a), [`-`](#link2d).	 [→index](#idx)

### `/=` : procedure/2

Usage: `(/= x y) => bool`

Return true if number `x` is not equal to `y`, nil otherwise.

See also: [`>`](#link3e), [`>=`](#link3e3d), [`<`](#link3c), [`<=`](#link3c3d).	 [→index](#idx)

### `<` : procedure/2

Usage: `(< x y) => bool`

Return true if `x` is smaller than `y.`

See also: [`<=`](#link3c3d), [`>=`](#link3e3d), [`>`](#link3e).	 [→index](#idx)

### `<=` : procedure/2

Usage: `(<= x y) => bool`

Return true if `x` is smaller than or equal to `y`, nil otherwise.

See also: [`>`](#link3e), [`<`](#link3c), [`>=`](#link3e3d), [`/=`](#link2f3d).	 [→index](#idx)

### `=` : procedure/2

Usage: `(= x y) => bool`

Return true if number `x` equals number `y`, nil otherwise.

See also: [`eql?`](#link65716c3f), [`equal?`](#link657175616c3f).	 [→index](#idx)

### `>` : procedure/2

Usage: `(> x y) => bool`

Return true if `x` is larger than `y`, nil otherwise.

See also: [`<`](#link3c), [`>=`](#link3e3d), [`<=`](#link3c3d), [`/=`](#link2f3d).	 [→index](#idx)

### `>=` : procedure/2

Usage: `(>= x y) => bool`

Return true if `x` is larger than or equal to `y`, nil otherwise.

See also: [`>`](#link3e), [`<`](#link3c), [`<=`](#link3c3d), [`/=`](#link2f3d).	 [→index](#idx)

### `abs` : procedure/1

Usage: `(abs x) => num`

Returns the absolute value of number `x.`

See also: [`*`](#link2a), [`-`](#link2d), [`+`](#link2b), [`/`](#link2f).	 [→index](#idx)

### `add1` : procedure/1

Usage: `(add1 n) => num`

Add 1 to number `n.`

See also: [`sub1`](#link73756231), [`+`](#link2b), [`-`](#link2d).	 [→index](#idx)

### `div` : procedure/2

Usage: `(div n k) => int`

Integer division of `n` by `k.`

See also: [`truncate`](#link7472756e63617465), [`/`](#link2f), [`int`](#link696e74).	 [→index](#idx)

### `even?` : procedure/1

Usage: `(even? n) => bool`

Returns true if the integer `n` is even, nil if it is not even.

See also: [`odd?`](#link6f64643f).	 [→index](#idx)

### `float` : procedure/1

Usage: `(float n) => float`

Convert `n` to a floating point value.

See also: [`int`](#link696e74).	 [→index](#idx)

### `int` : procedure/1

Usage: `(int n) => int`

Return `n` as an integer, rounding down to the nearest integer if necessary.

See also: [`float`](#link666c6f6174).	 [→index](#idx)

**Warning: If the number is very large this may result in returning the maximum supported integer number rather than the number as integer.**

### `max` : procedure/1 or more

Usage: `(max x1 x2 ...) => num`

Return the maximum of the given numbers.

See also: [`min`](#link6d696e), [`minmax`](#link6d696e6d6178).	 [→index](#idx)

### `min` : procedure/1 or more

Usage: `(min x1 x2 ...) => num`

Return the minimum of the given numbers.

See also: [`max`](#link6d6178), [`minmax`](#link6d696e6d6178).	 [→index](#idx)

### `minmax` : procedure/3

Usage: `(minmax pred li acc) => any`

Go through `li` and test whether for each `elem` the comparison (pred elem acc) is true. If so, `elem` becomes `acc`. Once all elements of the list have been compared, `acc` is returned. This procedure can be used to implement generalized minimum or maximum procedures.

See also: [`min`](#link6d696e), [`max`](#link6d6178).	 [→index](#idx)

### `mod` : procedure/2

Usage: `(mod x y) => num`

Compute `x` modulo `y.`

See also: [`%`](#link25), [`/`](#link2f).	 [→index](#idx)

### `odd?` : procedure/1

Usage: `(odd? n) => bool`

Returns true if the integer `n` is odd, nil otherwise.

See also: [`even?`](#link6576656e3f).	 [→index](#idx)

### `rand` : procedure/2

Usage: `(rand prng lower upper) => int`

Return a random integer in the interval [`lower`` upper`], both inclusive, from pseudo-random number generator `prng`. The `prng` argument must be an integer from 0 to 9 (inclusive).

See also: [`rnd`](#link726e64), [`rndseed`](#link726e6473656564).	 [→index](#idx)

### `rnd` : procedure/0

Usage: `(rnd prng) => num`

Return a random value in the interval [0, 1] from pseudo-random number generator `prng`. The `prng` argument must be an integer from 0 to 9 (inclusive).

See also: [`rand`](#link72616e64), [`rndseed`](#link726e6473656564).	 [→index](#idx)

### `rndseed` : procedure/1

Usage: `(rndseed prng n)`

Seed the pseudo-random number generator `prng` (0 to 9) with 64 bit integer value `n`. Larger values will be truncated. Seeding affects both the rnd and the rand function for the given `prng.`

See also: [`rnd`](#link726e64), [`rand`](#link72616e64).	 [→index](#idx)

### `sub1` : procedure/1

Usage: `(sub1 n) => num`

Subtract 1 from `n.`

See also: [`add1`](#link61646431), [`+`](#link2b), [`-`](#link2d).	 [→index](#idx)

### `truncate` : procedure/1 or more

Usage: `(truncate x [y]) => int`

Round down to nearest integer of `x`. If `y` is present, divide `x` by `y` and round down to the nearest integer.

See also: [`div`](#link646976), [`/`](#link2f), [`int`](#link696e74).	 [→index](#idx)



## Object-oriented Programming {#oop}

The OOP system uses arrays to store objects and also offers a more lightweight array-based structure system. It is not built for performance but may be useful to prevent writing object-oriented wrapper data structures again and again. This is also the reason why it was decided to embed the OOP system with a fixed API rather than providing it as an include file, allowing for interoperable object-oriented programming without having to worry about whether the extension is loaded. It's very simple and lightweight.

### `call-method` : procedure/3

Usage: `(call-method obj mname args) => any`

Execute method `mname` of object `obj` with additional arguments in list `args`. The first argument in the method call is always `obj` itself.

See also: [`defmethod`](#link6465666d6574686f64), [`defclass`](#link646566636c617373), [`new`](#link6e6577), [`isa?`](#link6973613f), [`class-of`](#link636c6173732d6f66).	 [→index](#idx)

### `call-super` : procedure/3

Usage: `(call-super obj mname args) => any`

Execute method `mname` of the first superclass of `obj` that has a method with that name.

See also: [`call-method`](#link63616c6c2d6d6574686f64), [`supers`](#link737570657273).	 [→index](#idx)

### `class-name` : procedure/1

Usage: `(class-name c) => sym`

Return the name of a class `c`. An error occurs if `c` is not a valid class.

See also: [`class?`](#link636c6173733f), [`isa?`](#link6973613f).	 [→index](#idx)

### `class-of` : procedure/1

Usage: `(class-of obj) => class or nil`

Return the class of object `obj`, nil if `obj` is not a valid object array.

See also: [`new`](#link6e6577), [`isa?`](#link6973613f).	 [→index](#idx)

### `class?` : procedure/1

Usage: `(class? c) => bool`

Return true if `c` is a class array (not a name for a class!), nil otherwise.

See also: [`object?`](#link6f626a6563743f), [`isa?`](#link6973613f).	 [→index](#idx)

### `copy-record` : procedure/1

Usage: `(copy-record r) => record`

Creates a non-recursive, shallow copy of record `r.`

See also: [`record?`](#link7265636f72643f).	 [→index](#idx)

### `defclass` : macro/2 or more

Usage: `(defclass name supers [props] ...)`

Defines symbol `name` as class with superclasses `supers` and property clauses `props` listed as remaining arguments. A `props` clause is either a symbol for a property or a list of the form (sym default) for the property `sym` with `default` value. The class is bound to `name` and a class predicate `name?` is created. Argument `supers` may be a class name or a list of class names.

See also: [`defmethod`](#link6465666d6574686f64), [`new`](#link6e6577).	 [→index](#idx)

### `defmethod` : macro/2 or more

Usage: `(defmethod class-name args [body] ...)`

Define a method `class-name` for class `class` and method name `name` with a syntax parallel to defun, where `args` are the arguments of the methods and `body` is the rest of the method. The given `class-name` must decompose into a valid class name `class` of a previously created class and method name `name` and is bound to the symbol `class-name`. The remaining arguments are like for defun. So for example (defmethod employee-name (this) (prop this 'last-name)) defines a method `name` for an existing class `employee` which retrieves the property `last-name`. Note that `defmethod` is dynamic: If you define a class B with class A as superclass, then B only inherits methods from A that have already been defined for A at the time of defining B!

See also: [`defclass`](#link646566636c617373), [`new`](#link6e6577), [`call-method`](#link63616c6c2d6d6574686f64).	 [→index](#idx)

### `defstruct` : macro/1 or more

Usage: `(defstruct name props ...) => struct`

Binds symbol `name` to a struct with name `name` and with properties `props`. Each clause of `props` must be either a symbol for the property name or a list of the form (prop default-value) where `prop` is the symbol for the property name and `default-value` is the value it has by default. For each property `p`, accessors `name-p` and setters `name-p!` are created, as well as a function `name-p*` that takes a record `r`, a value `v`, and a procedure `proc` that takes no arguments. When `name-p*` is called on record `r`, it temporarily sets property `p` of `r` to the provided value `v` and calls the procedure `proc`. Afterwards, the original value of `p` is restored. Since this function mutates the record during the execution of `proc` and does not protect this operation against race conditions, it is not thread-safe. (But you can include a mutex as property and make it thread-safe by wrapping it into `with-mutex-lock`.) The defstruct macro returns the struct that is bound to `name.`

See also: [`new-struct`](#link6e65772d737472756374), [`make`](#link6d616b65), [`with-mutex-lock`](#link776974682d6d757465782d6c6f636b).	 [→index](#idx)

### `has-method?` : procedure/2

Usage: `(has-method? obj name) => bool`

Return true if `obj` has a method with name `name`, nil otherwise.

See also: [`defmethod`](#link6465666d6574686f64), [`has-prop?`](#link6861732d70726f703f), [`new`](#link6e6577), [`props`](#link70726f7073), [`methods`](#link6d6574686f6473), [`prop`](#link70726f70), [`setprop`](#link73657470726f70).	 [→index](#idx)

### `has-prop?` : procedure/2

Usage: `(has-prop? obj slot) => bool`

Return true if `obj` has a property named `slot`, nil otherwise.

See also: [`has-method?`](#link6861732d6d6574686f643f), [`new`](#link6e6577), [`props`](#link70726f7073), [`methods`](#link6d6574686f6473), [`prop`](#link70726f70), [`setprop`](#link73657470726f70).	 [→index](#idx)

### `isa?` : procedure/2

Usage: `(isa? obj class) => bool`

Return true if `obj` is an instance of `class`, nil otherwise.

See also: [`supers`](#link737570657273).	 [→index](#idx)

### `make` : macro/2

Usage: `(make name props)`

Create a new record (struct instance) of struct `name` (unquoted) with properties `props`. Each clause in `props` must be a list of property name and initial value.

See also: [`make*`](#link6d616b652a), [`defstruct`](#link646566737472756374).	 [→index](#idx)

### `make*` : macro/1 or more

Usage: `(make* name prop1 ...)`

Create a new record (struct instance) of struct `name` (unquoted) with property clauses `prop-1` ... `prop-n`, where each clause is a list of property name and initial value like in `make.`

See also: [`make`](#link6d616b65), [`defstruct`](#link646566737472756374).	 [→index](#idx)

### `methods` : procedure/1

Usage: `(methods obj) => li`

Return the list of methods of `obj`, which must be a class, object, or class name.

See also: [`has-method?`](#link6861732d6d6574686f643f), [`new`](#link6e6577), [`props`](#link70726f7073), [`prop`](#link70726f70), [`setprop`](#link73657470726f70), [`has-prop?`](#link6861732d70726f703f).	 [→index](#idx)

### `new` : macro/1 or more

Usage: `(new class [props] ...)`

Create a new object of class `class` with initial property bindings `props` clauses as remaining arguments. Each `props` clause must be a list of the form (sym value), where `sym` is a symbol and `value` is evaluated first before it is assigned to `sym.`

See also: [`defclass`](#link646566636c617373).	 [→index](#idx)

### `new-struct` : procedure/2

Usage: `(new-struct name li)`

Defines a new structure `name` with the properties in the a-list `li`. Structs are more leightweight than classes and do not allow for inheritance. Instances of structs ("records") are arrays.

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx)

### `object?` : procedure/1

Usage: `(object? obj) => bool`

Return true of `obj` is an object array, nil otherwise.

See also: [`class?`](#link636c6173733f), [`isa?`](#link6973613f).	 [→index](#idx)

### `prop` : procedure/2

Usage: `(prop obj slot) => any`

Return the value in `obj` for property `slot`, or an error if the object does not have a property with that name.

See also: [`new`](#link6e6577), [`isa?`](#link6973613f), [`setprop`](#link73657470726f70), [`object?`](#link6f626a6563743f), [`class-name`](#link636c6173732d6e616d65), [`supers`](#link737570657273), [`props`](#link70726f7073), [`methods`](#link6d6574686f6473), [`has-prop?`](#link6861732d70726f703f).	 [→index](#idx)

### `props` : procedure/1

Usage: `(props obj) => li`

Return the list of properties of `obj`. An error occurs if `obj` is not a valid object.

See also: [`methods`](#link6d6574686f6473), [`has-prop?`](#link6861732d70726f703f), [`new`](#link6e6577), [`prop`](#link70726f70), [`setprop`](#link73657470726f70).	 [→index](#idx)

### `record?` : procedure/1

Usage: `(record? s) => bool`

Returns true if `s` is a struct record, i.e., an instance of a struct; nil otherwise. Notice that records are not really types distinct from arrays, they simply contain a marker '%record as first element. With normal use no confusion should arise. Since the internal representation might change, you ought not use ordinary array procedures for records.

See also: [`struct?`](#link7374727563743f), [`defstruct`](#link646566737472756374).	 [→index](#idx)

### `setprop` : procedure/3

Usage: `(setprop obj slot value)`

Set property `slot` in `obj` to `value`. An error occurs if the object does not have a property with that name.

See also: [`new`](#link6e6577), [`isa?`](#link6973613f), [`prop`](#link70726f70), [`object?`](#link6f626a6563743f), [`class-name`](#link636c6173732d6e616d65), [`supers`](#link737570657273), [`props`](#link70726f7073), [`methods`](#link6d6574686f6473), [`has-prop?`](#link6861732d70726f703f).	 [→index](#idx)

### `struct-index` : procedure/1

Usage: `(struct-index s) => dict`

Returns the index of struct `s` as a dict. This dict is an internal representation of the struct's instance data.

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx)

### `struct-instantiate` : procedure/2

Usage: `(struct-instantiate s li) => record`

Instantiates the struct `s` with property a-list `li` as values for its properties and return the record. If a property is not in `li`, its value is set to nil.

See also: [`make`](#link6d616b65), [`defstruct`](#link646566737472756374), [`struct?`](#link7374727563743f), [`record?`](#link7265636f72643f).	 [→index](#idx)

### `struct-name` : procedure/1

Usage: `(struct-name s) => sym`

Returns the name of a struct `s`. This is rarely needed since the struct is bound to a symbol with the same name.

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx)

### `struct-props` : procedure/1

Usage: `(struct-props s) => dict`

Returns the properties of structure `s` as dict.

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx)

### `struct-size` : procedure/1

Usage: `(strict-size s) => int`

Returns the number of properties of struct `s.`

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx)

### `struct?` : procedure/1

Usage: `(struct? datum) => boo`

Returns true if `datum` is a struct, nil otherwise.

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx)

### `supers` : procedure/1

Usage: `(supers c) => li`

Return the list of superclasses of class `c`. An error occurs if `c` is not a valid class.

See also: [`class?`](#link636c6173733f), [`isa?`](#link6973613f), [`class-name`](#link636c6173732d6e616d65).	 [→index](#idx)





## Semver Semantic Versioning {#semver}

The `semver` package provides functions to deal with the validation and parsing of semantic versioning strings.

### `semver.build` : procedure/1

Usage: `(semver.build s) => str`

Return the build part of a semantic versioning string.

See also: [`semver.canonical`](#link73656d7665722e63616e6f6e6963616c), [`semver.major`](#link73656d7665722e6d616a6f72), [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72).	 [→index](#idx)

### `semver.canonical` : procedure/1

Usage: `(semver.canonical s) => str`

Return a canonical semver string based on a valid, yet possibly not canonical version string `s.`

See also: [`semver.major`](#link73656d7665722e6d616a6f72).	 [→index](#idx)

### `semver.compare` : procedure/2

Usage: `(semver.compare s1 s2) => int`

Compare two semantic version strings `s1` and `s2`. The result is 0 if `s1` and `s2` are the same version, -1 if `s1` < `s2` and 1 if `s1` > `s2.`

See also: [`semver.major`](#link73656d7665722e6d616a6f72), [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72).	 [→index](#idx)

### `semver.is-valid?` : procedure/1

Usage: `(semver.is-valid? s) => bool`

Return true if `s` is a valid semantic versioning string, nil otherwise.

See also: [`semver.major`](#link73656d7665722e6d616a6f72), [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72), [`semver.compare`](#link73656d7665722e636f6d70617265).	 [→index](#idx)

### `semver.major` : procedure/1

Usage: `(semver.major s) => str`

Return the major part of the semantic versioning string.

See also: [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72), [`semver.build`](#link73656d7665722e6275696c64).	 [→index](#idx)

### `semver.major-minor` : procedure/1

Usage: `(semver.major-minor s) => str`

Return the major.minor prefix of a semantic versioning string. For example, (semver.major-minor "v2.1.4") returns "v2.1".

See also: [`semver.major`](#link73656d7665722e6d616a6f72), [`semver.build`](#link73656d7665722e6275696c64).	 [→index](#idx)

### `semver.max` : procedure/2

Usage: `(semver.max s1 s2) => str`

Canonicalize `s1` and `s2` and return the larger version of them.

See also: [`semver.compare`](#link73656d7665722e636f6d70617265).	 [→index](#idx)

### `semver.prerelease` : procedure/1

Usage: `(semver.prerelease s) => str`

Return the prerelease part of a version string, or the empty string if there is none. For example, (semver.prerelease "v2.1.0-pre+build") returns "-pre".

See also: [`semver.build`](#link73656d7665722e6275696c64), [`semver.major`](#link73656d7665722e6d616a6f72), [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72).	 [→index](#idx)



## Sequence Functions {#seq}

Sequences are either strings, lists, or arrays. Sequences functions are generally abstractions for more specific functions of these data types, and therefore may be a bit slower than their native counterparts. It is still recommended to use them liberally, since they make programs more readable.

### `10th` : procedure/1 or more

Usage: `(10th seq [default]) => any`

Get the tenth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468).	 [→index](#idx)

### `1st` : procedure/1 or more

Usage: `(1st seq [default]) => any`

Get the first element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `2nd` : procedure/1 or more

Usage: `(2nd seq [default]) => any`

Get the second element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `3rd` : procedure/1 or more

Usage: `(3rd seq [default]) => any`

Get the third element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `4th` : procedure/1 or more

Usage: `(4th seq [default]) => any`

Get the fourth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `5th` : procedure/1 or more

Usage: `(5th seq [default]) => any`

Get the fifth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `6th` : procedure/1 or more

Usage: `(6th seq [default]) => any`

Get the sixth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `7th` : procedure/1 or more

Usage: `(7th seq [default]) => any`

Get the seventh element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `8th` : procedure/1 or more

Usage: `(8th seq [default]) => any`

Get the eighth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `9th` : procedure/1 or more

Usage: `(9th seq [default]) => any`

Get the nineth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`10th`](#link31307468).	 [→index](#idx)

### `exists?` : procedure/2

Usage: `(exists? seq pred) => bool`

Return true if `pred` returns true for at least one element in sequence `seq`, nil otherwise.

See also: [`forall?`](#link666f72616c6c3f), [`list-exists?`](#link6c6973742d6578697374733f), [`array-exists?`](#link61727261792d6578697374733f), [`str-exists?`](#link7374722d6578697374733f), [`seq?`](#link7365713f).	 [→index](#idx)

### `forall?` : procedure/2

Usage: `(forall? seq pred) => bool`

Return true if predicate `pred` returns true for all elements of sequence `seq`, nil otherwise.

See also: [`foreach`](#link666f7265616368), [`map`](#link6d6170), [`list-forall?`](#link6c6973742d666f72616c6c3f), [`array-forall?`](#link61727261792d666f72616c6c3f), [`str-forall?`](#link7374722d666f72616c6c3f), [`exists?`](#link6578697374733f), [`str-exists?`](#link7374722d6578697374733f), [`array-exists?`](#link61727261792d6578697374733f), [`list-exists?`](#link6c6973742d6578697374733f).	 [→index](#idx)

### `foreach` : procedure/2

Usage: `(foreach seq proc)`

Apply `proc` to each element of sequence `seq` in order, for the side effects.

See also: [`seq?`](#link7365713f), [`map`](#link6d6170).	 [→index](#idx)

### `index` : procedure/2 or more

Usage: `(index seq elem [pred]) => int`

Return the first index of `elem` in `seq` going from left to right, using equality predicate `pred` for comparisons (default is eq?). If `elem` is not in `seq`, -1 is returned.

See also: [`nth`](#link6e7468), [`seq?`](#link7365713f).	 [→index](#idx)

### `last` : procedure/1 or more

Usage: `(last seq [default]) => any`

Get the last element of sequence `seq` or return `default` if the sequence is empty. If `default` is not given and the sequence is empty, an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string`](#link737472696e67), [`ref`](#link726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `len` : procedure/1

Usage: `(len seq) => int`

Return the length of `seq`. Works for lists, strings, arrays, and dicts.

See also: [`seq?`](#link7365713f).	 [→index](#idx)

### `map` : procedure/2

Usage: `(map seq proc) => seq`

Return the copy of `seq` that is the result of applying `proc` to each element of `seq.`

See also: [`seq?`](#link7365713f), [`mapcar`](#link6d6170636172), [`strmap`](#link7374726d6170).	 [→index](#idx)

### `map-pairwise` : procedure/2

Usage: `(map-pairwise seq proc) => seq`

Applies `proc` in order to subsequent pairs in `seq`, assembling the sequence that results from the results of `proc`. Function `proc` takes two arguments and must return a proper list containing two elements. If the number of elements in `seq` is odd, an error is raised.

See also: [`map`](#link6d6170).	 [→index](#idx)

### `nth` : procedure/2

Usage: `(nth seq n) => any`

Get the `n-th` element of sequence `seq`. Sequences are 0-indexed.

See also: [`nthdef`](#link6e7468646566), [`list`](#link6c697374), [`array`](#link6172726179), [`string`](#link737472696e67), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `nthdef` : procedure/3

Usage: `(nthdef seq n default) => any`

Return the `n-th` element of sequence `seq` (0-indexed) if `seq` is a sequence and has at least `n+1` elements, default otherwise.

See also: [`nth`](#link6e7468), [`seq?`](#link7365713f), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx)

### `remove-duplicates` : procedure/1

Usage: `(remove-duplicates seq) => seq`

Remove all duplicates in sequence `seq`, return a new sequence with the duplicates removed.

See also: [`seq?`](#link7365713f), [`map`](#link6d6170), [`foreach`](#link666f7265616368), [`nth`](#link6e7468).	 [→index](#idx)

### `reverse` : procedure/1

Usage: `(reverse seq) => sequence`

Reverse a sequence non-destructively, i.e., return a copy of the reversed sequence.

See also: [`nth`](#link6e7468), [`seq?`](#link7365713f), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468), [`last`](#link6c617374).	 [→index](#idx)

### `seq?` : procedure/1

Usage: `(seq? seq) => bool`

Return true if `seq` is a sequence, nil otherwise.

See also: [`list`](#link6c697374), [`array`](#link6172726179), [`string`](#link737472696e67), [`slice`](#link736c696365), [`nth`](#link6e7468).	 [→index](#idx)

### `slice` : procedure/3

Usage: `(slice seq low high) => seq`

Return the subsequence of `seq` starting from `low` inclusive and ending at `high` exclusive. Sequences are 0-indexed.

See also: [`list`](#link6c697374), [`array`](#link6172726179), [`string`](#link737472696e67), [`nth`](#link6e7468), [`seq?`](#link7365713f).	 [→index](#idx)

### `take` : procedure/3

Usage: `(take seq n) => seq`

Return the sequence consisting of the `n` first elements of `seq.`

See also: [`list`](#link6c697374), [`array`](#link6172726179), [`string`](#link737472696e67), [`nth`](#link6e7468), [`seq?`](#link7365713f).	 [→index](#idx)









## Sound Support {#sound}

Only a few functions are provided for sound support.

### `beep` : procedure/1

Usage: `(beep sel)`

Play a built-in system sound. The argument `sel` may be one of '(error start ready click okay confirm info).

See also: [`set-volume`](#link7365742d766f6c756d65).	 [→index](#idx)

### `set-volume` : procedure/1

Usage: `(set-volume fl)`

Set the master volume for all sound to `fl`, a value between 0.0 and 1.0.

See also: [`beep`](#link62656570).	 [→index](#idx)





## String Manipulation {#str}

These functions all manipulate strings in one way or another.

### `fmt` : procedure/1 or more

Usage: `(fmt s [args] ...) => str`

Format string `s` that contains format directives with arbitrary many `args` as arguments. The number of format directives must match the number of arguments. The format directives are the same as those for the esoteric and arcane programming language "Go", which was used on Earth for some time.

See also: [`out`](#link6f7574).	 [→index](#idx)

### `instr` : procedure/2

Usage: `(instr s1 s2) => int`

Return the index of the first occurrence of `s2` in `s1` (from left), or -1 if `s1` does not contain `s2.`

See also: [`str?`](#link7374723f), [`index`](#link696e646578).	 [→index](#idx)

### `shorten` : procedure/2

Usage: `(shorten s n) => str`

Shorten string `s` to length `n` in a smart way if possible, leave it untouched if the length of `s` is smaller than `n.`

See also: [`substr`](#link737562737472).	 [→index](#idx)

### `spaces` : procedure/1

Usage: `(spaces n) => str`

Create a string consisting of `n` spaces.

See also: [`strbuild`](#link7374726275696c64), [`strleft`](#link7374726c656674), [`strright`](#link7374727269676874).	 [→index](#idx)

### `str+` : procedure/0 or more

Usage: `(str+ [s] ...) => str`

Append all strings given to the function.

See also: [`str?`](#link7374723f).	 [→index](#idx)

### `str-count-substr` : procedure/2

Usage: `(str-count-substr s1 s2) => int`

Count the number of non-overlapping occurrences of substring `s2` in string `s1.`

See also: [`str-replace`](#link7374722d7265706c616365), [`str-replace*`](#link7374722d7265706c6163652a), [`instr`](#link696e737472).	 [→index](#idx)

### `str-empty?` : procedure/1

Usage: `(str-empty? s) => bool`

Return true if the string `s` is empty, nil otherwise.

See also: [`strlen`](#link7374726c656e).	 [→index](#idx)

### `str-exists?` : procedure/2

Usage: `(str-exists? s pred) => bool`

Return true if `pred` returns true for at least one character in string `s`, nil otherwise.

See also: [`exists?`](#link6578697374733f), [`forall?`](#link666f72616c6c3f), [`list-exists?`](#link6c6973742d6578697374733f), [`array-exists?`](#link61727261792d6578697374733f), [`seq?`](#link7365713f).	 [→index](#idx)

### `str-forall?` : procedure/2

Usage: `(str-forall? s pred) => bool`

Return true if predicate `pred` returns true for all characters in string `s`, nil otherwise.

See also: [`foreach`](#link666f7265616368), [`map`](#link6d6170), [`forall?`](#link666f72616c6c3f), [`array-forall?`](#link61727261792d666f72616c6c3f), [`list-forall`](#link6c6973742d666f72616c6c), [`exists?`](#link6578697374733f).	 [→index](#idx)

### `str-foreach` : procedure/2

Usage: `(str-foreach s proc)`

Apply `proc` to each element of string `s` in order, for the side effects.

See also: [`foreach`](#link666f7265616368), [`list-foreach`](#link6c6973742d666f7265616368), [`array-foreach`](#link61727261792d666f7265616368), [`map`](#link6d6170).	 [→index](#idx)

### `str-index` : procedure/2 or more

Usage: `(str-index s chars [pos]) => int`

Find the first char in `s` that is in the charset `chars`, starting from the optional `pos` in `s`, and return its index in the string. If no macthing char is found, nil is returned.

See also: [`strsplit`](#link73747273706c6974), [`chars`](#link6368617273), [`inchars`](#link696e6368617273).	 [→index](#idx)

### `str-join` : procedure/2

Usage: `(str-join li del) => str`

Join a list of strings `li` where each of the strings is separated by string `del`, and return the result string.

See also: [`strlen`](#link7374726c656e), [`strsplit`](#link73747273706c6974), [`str-slice`](#link7374722d736c696365).	 [→index](#idx)

### `str-ref` : procedure/2

Usage: `(str-ref s n) => n`

Return the unicode char as integer at position `n` in `s`. Strings are 0-indexed.

See also: [`nth`](#link6e7468).	 [→index](#idx)

### `str-remove-number` : procedure/1

Usage: `(str-remove-number s [del]) => str`

Remove the suffix number in `s`, provided there is one and it is separated from the rest of the string by `del`, where the default is a space character. For instance, "Test 29" will be converted to "Test", "User-Name1-23-99" with delimiter "-" will be converted to "User-Name1-23". This function will remove intermediate delimiters in the middle of the string, since it disassembles and reassembles the string, so be aware that this is not preserving inputs in that respect.

See also: [`strsplit`](#link73747273706c6974).	 [→index](#idx)

### `str-remove-prefix` : procedure/1

Usage: `(str-remove-prefix s prefix) => str`

Remove the prefix `prefix` from string `s`, return the string without the prefix. If the prefix does not match, `s` is returned. If `prefix` is longer than `s` and matches, the empty string is returned.

See also: [`str-remove-suffix`](#link7374722d72656d6f76652d737566666978).	 [→index](#idx)

### `str-remove-suffix` : procedure/1

Usage: `(str-remove-suffix s suffix) => str`

remove the suffix `suffix` from string `s`, return the string without the suffix. If the suffix does not match, `s` is returned. If `suffix` is longer than `s` and matches, the empty string is returned.

See also: [`str-remove-prefix`](#link7374722d72656d6f76652d707265666978).	 [→index](#idx)

### `str-replace` : procedure/4

Usage: `(str-replace s t1 t2 n) => str`

Replace the first `n` instances of substring `t1` in `s` by `t2.`

See also: [`str-replace*`](#link7374722d7265706c6163652a), [`str-count-substr`](#link7374722d636f756e742d737562737472).	 [→index](#idx)

### `str-replace*` : procedure/3

Usage: `(str-replace* s t1 t2) => str`

Replace all non-overlapping substrings `t1` in `s` by `t2.`

See also: [`str-replace`](#link7374722d7265706c616365), [`str-count-substr`](#link7374722d636f756e742d737562737472).	 [→index](#idx)

### `str-reverse` : procedure/1

Usage: `(str-reverse s) => str`

Reverse string `s.`

See also: [`reverse`](#link72657665727365), [`array-reverse`](#link61727261792d72657665727365), [`list-reverse`](#link6c6973742d72657665727365).	 [→index](#idx)

### `str-segment` : procedure/3

Usage: `(str-segment str start end) => list`

Parse a string `str` into words that start with one of the characters in string `start` and end in one of the characters in string `end` and return a list consisting of lists of the form (bool s) where bool is true if the string starts with a character in `start`, nil otherwise, and `s` is the extracted string including start and end characters.

See also: [`str+`](#link7374722b), [`strsplit`](#link73747273706c6974), [`fmt`](#link666d74), [`strbuild`](#link7374726275696c64).	 [→index](#idx)

### `str-slice` : procedure/3

Usage: `(str-slice s low high) => s`

Return a slice of string `s` starting at character with index `low` (inclusive) and ending at character with index `high` (exclusive).

See also: [`slice`](#link736c696365).	 [→index](#idx)

### `strbuild` : procedure/2

Usage: `(strbuild s n) => str`

Build a string by repeating string `s`` n` times.

See also: [`str+`](#link7374722b).	 [→index](#idx)

### `strcase` : procedure/2

Usage: `(strcase s sel) => str`

Change the case of the string `s` according to selector `sel` and return a copy. Valid values for `sel` are 'lower for conversion to lower-case, 'upper for uppercase, 'title for title case and 'utf-8 for utf-8 normalization (which replaces unprintable characters with "?").

See also: [`strmap`](#link7374726d6170).	 [→index](#idx)

### `strcenter` : procedure/2

Usage: `(strcenter s n) => str`

Center string `s` by wrapping space characters around it, such that the total length the result string is `n.`

See also: [`strleft`](#link7374726c656674), [`strright`](#link7374727269676874), [`strlimit`](#link7374726c696d6974).	 [→index](#idx)

### `strcnt` : procedure/2

Usage: `(strcnt s del) => int`

Returnt the number of non-overlapping substrings `del` in `s.`

See also: [`strsplit`](#link73747273706c6974), [`str-index`](#link7374722d696e646578).	 [→index](#idx)

### `strleft` : procedure/2

Usage: `(strleft s n) => str`

Align string `s` left by adding space characters to the right of it, such that the total length the result string is `n.`

See also: [`strcenter`](#link73747263656e746572), [`strright`](#link7374727269676874), [`strlimit`](#link7374726c696d6974).	 [→index](#idx)

### `strlen` : procedure/1

Usage: `(strlen s) => int`

Return the length of `s.`

See also: [`len`](#link6c656e), [`seq?`](#link7365713f), [`str?`](#link7374723f).	 [→index](#idx)

### `strless` : procedure/2

Usage: `(strless s1 s2) => bool`

Return true if string `s1` < `s2` in lexicographic comparison, nil otherwise.

See also: [`sort`](#link736f7274), [`array-sort`](#link61727261792d736f7274), [`strcase`](#link73747263617365).	 [→index](#idx)

### `strlimit` : procedure/2

Usage: `(strlimit s n) => str`

Return a string based on `s` cropped to a maximal length of `n` (or less if `s` is shorter).

See also: [`strcenter`](#link73747263656e746572), [`strleft`](#link7374726c656674), [`strright`](#link7374727269676874).	 [→index](#idx)

### `strmap` : procedure/2

Usage: `(strmap s proc) => str`

Map function `proc`, which takes a number and returns a number, over all unicode characters in `s` and return the result as new string.

See also: [`map`](#link6d6170).	 [→index](#idx)

### `stropen` : procedure/1

Usage: `(stropen s) => streamport`

Open the string `s` as input stream.

See also: [`open`](#link6f70656e), [`close`](#link636c6f7365).	 [→index](#idx)

### `strright` : procedure/2

Usage: `(strright s n) => str`

Align string `s` right by adding space characters in front of it, such that the total length the result string is `n.`

See also: [`strcenter`](#link73747263656e746572), [`strleft`](#link7374726c656674), [`strlimit`](#link7374726c696d6974).	 [→index](#idx)

### `strsplit` : procedure/2

Usage: `(strsplit s del) => array`

Return an array of strings obtained from `s` by splitting `s` at each occurrence of string `del.`

See also: [`str?`](#link7374723f).	 [→index](#idx)



## System Functions {#system}

These functions concern the inner workings of the Lisp interpreter. Your warranty might be void if you abuse them!

### *error-handler* : dict

Usage: `(*error-handler* err)`

The global error handler dict that contains procedures which take an error and handle it. If an entry is nil, the default handler is used, which outputs the error using *error-printer*. The dict contains handlers based on concurrent thread IDs and ought not be manipulated directly.

See also: [`*error-printer*`](#link2a6572726f722d7072696e7465722a).	 [→index](#idx)

### `*error-printer*` : procedure/1

Usage: `(*error-printer* err)`

The global printer procedure which takes an error and prints it.

See also: [`error`](#link6572726f72).	 [→index](#idx)

### *last-error* : sym

Usage: `*last-error* => str`

Contains the last error that has occurred.

See also: [`*error-printer*`](#link2a6572726f722d7072696e7465722a), [`*error-handler*`](#link2a6572726f722d68616e646c65722a).	 [→index](#idx)

**Warning: This may only be used for debugging! Do *not* use this for error handling, it will surely fail!**

### *reflect* : symbol

Usage: `*reflect* => li`

The list of feature identifiers as symbols that this Lisp implementation supports.

See also: [`feature?`](#link666561747572653f), [`on-feature`](#link6f6e2d66656174757265).	 [→index](#idx)

### `add-hook` : procedure/2

Usage: `(add-hook hook proc) => id`

Add hook procedure `proc` which takes a list of arguments as argument under symbolic or numeric `hook` and return an integer hook `id` for this hook. If `hook` is not known, nil is returned.

See also: [`remove-hook`](#link72656d6f76652d686f6f6b), [`remove-hooks`](#link72656d6f76652d686f6f6b73), [`replace-hook`](#link7265706c6163652d686f6f6b).	 [→index](#idx)

### `add-hook-internal` : procedure/2

Usage: `(add-hook-internal hook proc) => int`

Add a procedure `proc` to hook with numeric ID `hook` and return this procedures hook ID. The function does not check whether the hook exists.

See also: [`add-hook`](#link6164642d686f6f6b).	 [→index](#idx)

**Warning: Internal use only.**

### `add-hook-once` : procedure/2

Usage: `(add-hook-once hook proc) => id`

Add a hook procedure `proc` which takes a list of arguments under symbolic or numeric `hook` and return an integer hook `id`. If `hook` is not known, nil is returned.

See also: [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b), [`replace-hook`](#link7265706c6163652d686f6f6b).	 [→index](#idx)

### `bind` : procedure/2

Usage: `(bind sym value)`

Bind `value` to the global symbol `sym`. In contrast to setq both values need quoting.

See also: [`setq`](#link73657471).	 [→index](#idx)

### `bound?` : macro/1

Usage: `(bound? sym) => bool`

Return true if a value is bound to the symbol `sym`, nil otherwise.

See also: [`bind`](#link62696e64), [`setq`](#link73657471).	 [→index](#idx)

### `boxed?` : procedure/1

Usage: `(boxed? x) => bool`

Return true if `x` is a boxed value, nil otherwise. Boxed values are special objects that are special in the system and sometimes cannot be garbage collected.

See also: [`type-of`](#link747970652d6f66), [`num?`](#link6e756d3f), [`str?`](#link7374723f), [`sym?`](#link73796d3f), [`list?`](#link6c6973743f), [`array?`](#link61727261793f), [`macro?`](#link6d6163726f3f), [`closure?`](#link636c6f737572653f), [`intrinsic?`](#link696e7472696e7369633f), [`eof?`](#link656f663f).	 [→index](#idx)

### `can-externalize?` : procedure/1

Usage: `(can-externalize? datum) => bool`

Recursively determines if `datum` can be externalized and returns true in this case, nil otherwise.

See also: [`externalize`](#link65787465726e616c697a65), [`externalize0`](#link65787465726e616c697a6530).	 [→index](#idx)

### `closure?` : procedure/1

Usage: `(closure? x) => bool`

Return true if `x` is a closure, nil otherwise. Use `function?` for texting whether `x` can be executed.

See also: [`functional?`](#link66756e6374696f6e616c3f), [`macro?`](#link6d6163726f3f), [`intrinsic?`](#link696e7472696e7369633f), [`functional-arity`](#link66756e6374696f6e616c2d6172697479), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx)

### `collect-garbage` : procedure/0 or more

Usage: `(collect-garbage [sort])`

Force a garbage-collection of the system's memory. If `sort` is 'normal, then only a normal incremental garbage colllection is performed. If `sort` is 'total, then the garbage collection is more thorough and the system attempts to return unused memory to the host OS. Default is 'normal.

See also: [`memstats`](#link6d656d7374617473).	 [→index](#idx)

**Warning: There should rarely be a use for this. Try to use less memory-consuming data structures instead.**

### `current-error-handler` : procedure/0

Usage: `(current-error-handler) => proc`

Return the current error handler, a default if there is none.

See also: [`default-error-handler`](#link64656661756c742d6572726f722d68616e646c6572), [`push-error-handler`](#link707573682d6572726f722d68616e646c6572), [`pop-error-handler`](#link706f702d6572726f722d68616e646c6572), [`*current-error-handler*`](#link2a63757272656e742d6572726f722d68616e646c65722a), [`*current-error-continuation*`](#link2a63757272656e742d6572726f722d636f6e74696e756174696f6e2a).	 [→index](#idx)

### `def-custom-hook` : procedure/2

Usage: `(def-custom-hook sym proc)`

Define a custom hook point, to be called manually from Lisp. These have IDs starting from 65636.

See also: [`add-hook`](#link6164642d686f6f6b).	 [→index](#idx)

### `default-error-handler` : procedure/0

Usage: `(default-error-handler) => proc`

Return the default error handler, irrespectively of the current-error-handler.

See also: [`current-error-handler`](#link63757272656e742d6572726f722d68616e646c6572), [`push-error-handler`](#link707573682d6572726f722d68616e646c6572), [`pop-error-handler`](#link706f702d6572726f722d68616e646c6572), [`*current-error-handler*`](#link2a63757272656e742d6572726f722d68616e646c65722a), [`*current-error-continuation*`](#link2a63757272656e742d6572726f722d636f6e74696e756174696f6e2a).	 [→index](#idx)

### `dict-protect` : procedure/1

Usage: `(dict-protect d)`

Protect dict `d` against changes. Attempting to set values in a protected dict will cause an error, but all values can be read and the dict can be copied. This function requires permission 'allow-protect.

See also: [`dict-unprotect`](#link646963742d756e70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`protected?`](#link70726f7465637465643f), [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f).	 [→index](#idx)

**Warning: Protected dicts are full readable and can be copied, so you may need to use protect to also prevent changes to the toplevel symbol storing the dict!**

### `dict-protected?` : procedure/1

Usage: `(dict-protected? d)`

Return true if the dict `d` is protected against mutation, nil otherwise.

See also: [`dict-protect`](#link646963742d70726f74656374), [`dict-unprotect`](#link646963742d756e70726f74656374), [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`protected?`](#link70726f7465637465643f), [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f).	 [→index](#idx)

### `dict-unprotect` : procedure/1

Usage: `(dict-unprotect d)`

Unprotect the dict `d` so it can be mutated again. This function requires permission 'allow-unprotect.

See also: [`dict-protect`](#link646963742d70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`protected?`](#link70726f7465637465643f), [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f).	 [→index](#idx)

### `dump` : procedure/0 or more

Usage: `(dump [sym] [all?]) => li`

Return a list of symbols starting with the characters of `sym` or starting with any characters if `sym` is omitted, sorted alphabetically. When `all?` is true, then all symbols are listed, otherwise only symbols that do not contain "_" are listed. By convention, the underscore is used for auxiliary functions.

See also: [`dump-bindings`](#link64756d702d62696e64696e6773), [`save-zimage`](#link736176652d7a696d616765), [`load-zimage`](#link6c6f61642d7a696d616765).	 [→index](#idx)

### `dump-bindings` : procedure/0

Usage: `(dump-bindings) => li`

Return a list of all top-level symbols with bound values, including those intended for internal use.

See also: [`dump`](#link64756d70).	 [→index](#idx)

### `error` : procedure/0 or more

Usage: `(error [msgstr] [expr] ...)`

Raise an error, where `msgstr` and the optional expressions `expr`... work as in a call to fmt.

See also: [`fmt`](#link666d74), [`with-final`](#link776974682d66696e616c).	 [→index](#idx)

### `error->str` : procedure/1

Usage: `(error->str datum) => str`

Convert a special error value to a string.

See also: [`*last-error*`](#link2a6c6173742d6572726f722a), [`error`](#link6572726f72), [`error?`](#link6572726f723f).	 [→index](#idx)

### `error?` : procedure/1

Usage: `(error? datum) => bool`

Return true if `datum` is a special error value, nil otherwise.

See also: [`*last-error*`](#link2a6c6173742d6572726f722a), [`error->str`](#link6572726f722d3e737472), [`error`](#link6572726f72), [`eof?`](#link656f663f), [`valid?`](#link76616c69643f).	 [→index](#idx)

### `eval` : procedure/1

Usage: `(eval expr) => any`

Evaluate the expression `expr` in the Z3S5 Machine Lisp interpreter and return the result. The evaluation environment is the system's environment at the time of the call.

See also: [`break`](#link627265616b), [`apply`](#link6170706c79).	 [→index](#idx)

### `exit` : procedure/0 or more

Usage: `(exit [n])`

Immediately shut down the system and return OS host error code `n`. The shutdown is performed gracefully and exit hooks are executed.

See also: .	 [→index](#idx)

### `expand-macros` : procedure/1

Usage: `(expand-macros expr) => expr`

Expands the macros in `expr`. This is an ordinary function and will not work on already compiled expressions such as a function bound to a symbol. However, it can be used to expand macros in expressions obtained by `read.`

See also: [`internalize`](#link696e7465726e616c697a65), [`externalize`](#link65787465726e616c697a65), [`load-library`](#link6c6f61642d6c696272617279).	 [→index](#idx)

### `expect` : macro/2

Usage: `(expect value given)`

Registers a test under the current test name that checks that `value` is returned by `given`. The test is only executed when (run-selftest) is executed.

See also: [`expect-err`](#link6578706563742d657272), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374), [`testing`](#link74657374696e67).	 [→index](#idx)

### `expect-err` : macro/1 or more

Usage: `(expect-err expr ...)`

Registers a test under the current test name that checks that `expr` produces an error.

See also: [`expect`](#link657870656374), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374), [`testing`](#link74657374696e67).	 [→index](#idx)

### `expect-false` : macro/1 or more

Usage: `(expect-false expr ...)`

Registers a test under the current test name that checks that `expr` is nil.

See also: [`expect`](#link657870656374), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374), [`testing`](#link74657374696e67).	 [→index](#idx)

### `expect-ok` : macro/1 or more

Usage: `(expect-ok expr ...)`

Registers a test under the current test name that checks that `expr` does not produce an error.

See also: [`expect`](#link657870656374), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374), [`testing`](#link74657374696e67).	 [→index](#idx)

### `expect-true` : macro/1 or more

Usage: `(expect-true expr ...)`

Registers a test under the current test name that checks that `expr` is true (not nil).

See also: [`expect`](#link657870656374), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374), [`testing`](#link74657374696e67).	 [→index](#idx)

### `externalize` : procedure/1

Usage: `(externalize sym [nonce]) => sexpr`

Obtain an external representation of top-level symbol `sym`. The optional `nonce` must be a value unique in each system zimage, in order to distinguish data from procedures.

See also: [`can-externalize?`](#link63616e2d65787465726e616c697a653f), [`externalize0`](#link65787465726e616c697a6530), [`current-zimage`](#link63757272656e742d7a696d616765), [`save-zimage`](#link736176652d7a696d616765), [`load-zimage`](#link6c6f61642d7a696d616765).	 [→index](#idx)

### `externalize0` : procedure/1

Usage: `(externalize0 arg) => any`

Attempts to externalize `arg` but falls back to the internal expression if `arg` cannot be externalized. This procedure never fails but `can-externalize?` may be false for the result. This function is only used in miscellaneous printing. Use `externalize` to externalize expressions for writing to disk.

See also: [`externalize`](#link65787465726e616c697a65), [`can-externalize?`](#link63616e2d65787465726e616c697a653f).	 [→index](#idx)

### `feature?` : procedure/1

Usage: `(feature? sym) => bool`

Return true if the Lisp feature identified by symbol `sym` is available, nil otherwise.

See also: [`*reflect*`](#link2a7265666c6563742a), [`on-feature`](#link6f6e2d66656174757265).	 [→index](#idx)

### `file-display-name` : procedure/1

Usage: `(file-display-name s) => str`

Return the name part of a file path without extension.

See also: [`file-name`](#link66696c652d6e616d65), [`file-ext`](#link66696c652d657874), [`file-path`](#link66696c652d70617468).	 [→index](#idx)

### `file-name` : procedure/1

Usage: `(file-name s) => str`

Return the name part of a file path.

See also: [`file-display-name`](#link66696c652d646973706c61792d6e616d65), [`file-ext`](#link66696c652d657874), [`file-path`](#link66696c652d70617468).	 [→index](#idx)

### `file-path` : procedure/1

Usage: `(file-path s) => str`

Return the directory part of a file path.

See also: [`file-name`](#link66696c652d6e616d65), [`file-ext`](#link66696c652d657874), [`file-display-name`](#link66696c652d646973706c61792d6e616d65), [`open`](#link6f70656e).	 [→index](#idx)

### `file-suffix` : procedure/1

Usage: `(file-suffix s) => str`

Return the suffix of a file path without including the dot.

See also: [`file-display-name`](#link66696c652d646973706c61792d6e616d65), [`file-name`](#link66696c652d6e616d65), [`file-path`](#link66696c652d70617468).	 [→index](#idx)

### `find-missing-help-entries` : procedure/0

Usage: `(find-missing-help-entries) => li`

Return a list of global symbols for which help entries are missing.

See also: [`dump`](#link64756d70), [`dump-bindings`](#link64756d702d62696e64696e6773), [`find-unneeded-help-entries`](#link66696e642d756e6e65656465642d68656c702d656e7472696573), [`find-help-entries-with-incorrect-arity`](#link66696e642d68656c702d656e74726965732d776974682d696e636f72726563742d6172697479).	 [→index](#idx)

### `find-unneeded-help-entries` : procedure/0

Usage: `(find-unneeded-help-entries) => li`

Return a list of help entries for which no symbols are defined.

See also: [`dump`](#link64756d70), [`dump-bindings`](#link64756d702d62696e64696e6773), [`find-missing-help-entries`](#link66696e642d6d697373696e672d68656c702d656e7472696573), [`find-help-entries-with-incorrect-arity`](#link66696e642d68656c702d656e74726965732d776974682d696e636f72726563742d6172697479).	 [→index](#idx)

**Warning: This function returns false positives! Special forms like setq and macro are listed even though they clearly are useful and should have a help entry.**

### `functional-arity` : procedure/1

Usage: `(functional-arity proc) => int`

Return the arity of a functional `proc.`

See also: [`functional-arity*`](#link66756e6374696f6e616c2d61726974792a), [`functional?`](#link66756e6374696f6e616c3f), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx)

### `functional-arity*` : procedure/1

Usage: `(functional-arity* proc) => int`

Return the extended arity of a functional `proc`, which is positive for fixed number of arguments like `functional-arity` and -1 for 0 or more arguments, -2 for 1 or more arguments, -3 for 2 or more arguments, and so on.

See also: [`functional-arity`](#link66756e6374696f6e616c2d6172697479), [`functional?`](#link66756e6374696f6e616c3f), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx)

### `functional-has-rest?` : procedure/1

Usage: `(functional-has-rest? proc) => bool`

Return true if the functional `proc` has a &rest argument, nil otherwise.

See also: [`functional?`](#link66756e6374696f6e616c3f), [`functional-arity`](#link66756e6374696f6e616c2d6172697479).	 [→index](#idx)

### `functional?` : macro/1

Usage: `(functional? arg) => bool`

Return true if `arg` is either a builtin function, a closure, or a macro, nil otherwise. This is the right predicate for testing whether the argument is applicable and has an arity.

See also: [`closure?`](#link636c6f737572653f), [`proc?`](#link70726f633f), [`functional-arity`](#link66756e6374696f6e616c2d6172697479), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx)

### `gensym` : procedure/0

Usage: `(gensym) => sym`

Return a new symbol guaranteed to be unique during runtime.

See also: [`nonce`](#link6e6f6e6365).	 [→index](#idx)

### `global-startup-time` : procedure/0

Usage: `(global-startup-time) => num`

Return the global startup time in milliseconds. This is the time that the initial embedded init.lisp system required for booting, rounded to two decimal places unless it is 1 or less.

See also: [`now-ns`](#link6e6f772d6e73), [`time`](#link74696d65), [`now`](#link6e6f77).	 [→index](#idx)

### `hook` : procedure/1

Usage: `(hook symbol)`

Lookup the internal hook number from a symbolic name.

See also: [`*hooks*`](#link2a686f6f6b732a), [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b), [`remove-hooks`](#link72656d6f76652d686f6f6b73).	 [→index](#idx)

### `include` : procedure/1

Usage: `(include fi) => any`

Evaluate the lisp file `fi` one expression after the other in the current environment.

See also: [`read`](#link72656164), [`write`](#link7772697465), [`open`](#link6f70656e), [`close`](#link636c6f7365).	 [→index](#idx)

### `intern` : procedure/1

Usage: `(intern s) => sym`

Create a new interned symbol based on string `s.`

See also: [`gensym`](#link67656e73796d), [`str->sym`](#link7374722d3e73796d), [`make-symbol`](#link6d616b652d73796d626f6c).	 [→index](#idx)

### `internalize` : procedure/2

Usage: `(internalize arg nonce)`

Internalize an external representation of `arg`, using `nonce` for distinguishing between data and code that needs to be evaluated.

See also: [`externalize`](#link65787465726e616c697a65).	 [→index](#idx)

### `intrinsic` : procedure/1

Usage: `(intrinsic sym) => any`

Attempt to obtain the value that is intrinsically bound to `sym`. Use this function to express the intention to use the pre-defined builtin value of a symbol in the base language.

See also: [`bind`](#link62696e64), [`unbind`](#link756e62696e64).	 [→index](#idx)

**Warning: This function currently only returns the binding but this behavior might change in future.**

### `intrinsic?` : procedure/1

Usage: `(intrinsic? x) => bool`

Return true if `x` is an intrinsic built-in function, nil otherwise. Notice that this function tests the value and not that a symbol has been bound to the intrinsic.

See also: [`functional?`](#link66756e6374696f6e616c3f), [`macro?`](#link6d6163726f3f), [`closure?`](#link636c6f737572653f).	 [→index](#idx)

**Warning: What counts as an intrinsic or not may change from version to version. This is for internal use only.**

### `macro?` : procedure/1

Usage: `(macro? x) => bool`

Return true if `x` is a macro, nil otherwise.

See also: [`functional?`](#link66756e6374696f6e616c3f), [`intrinsic?`](#link696e7472696e7369633f), [`closure?`](#link636c6f737572653f), [`functional-arity`](#link66756e6374696f6e616c2d6172697479), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx)

### `make-symbol` : procedure/1

Usage: `(make-symbol s) => sym`

Create a new symbol based on string `s.`

See also: [`str->sym`](#link7374722d3e73796d).	 [→index](#idx)

### `memstats` : procedure/0

Usage: `(memstats) => dict`

Return a dict with detailed memory statistics for the system.

See also: [`collect-garbage`](#link636f6c6c6563742d67617262616765).	 [→index](#idx)

### `nonce` : procedure/0

Usage: `(nonce) => str`

Return a unique random string. This is not cryptographically secure but the string satisfies reasonable GUID requirements.

See also: [`externalize`](#link65787465726e616c697a65), [`internalize`](#link696e7465726e616c697a65).	 [→index](#idx)

### `on-feature` : macro/1 or more

Usage: `(on-feature sym body ...) => any`

Evaluate the expressions of `body` if the Lisp feature `sym` is supported by this implementation, do nothing otherwise.

See also: [`feature?`](#link666561747572653f), [`*reflect*`](#link2a7265666c6563742a).	 [→index](#idx)

### `permission?` : procedure/1

Usage: `(permission? sym [default]) => bool`

Return true if the permission for `sym` is set, nil otherwise. If the permission flag is unknown, then `default` is returned. The default for `default` is nil.

See also: [`permissions`](#link7065726d697373696f6e73), [`set-permissions`](#link7365742d7065726d697373696f6e73), [`when-permission`](#link7768656e2d7065726d697373696f6e), [`sys`](#link737973).	 [→index](#idx)

### `permissions` : procedure/0

Usage: `(permissions)`

Return a list of all active permissions of the current interpreter. Permissions are: `load-prelude` - load the init file on start; `load-user-init` - load the local user init on startup, file if present; `allow-unprotect` - allow the user to unprotect protected symbols (for redefining them); `allow-protect` - allow the user to protect symbols from redefinition or unbinding; `interactive` - make the session interactive, this is particularly used during startup to determine whether hooks are installed and feedback is given. Permissions have to generally be set or removed in careful combination with `revoke-permissions`, which redefines symbols and functions.

See also: [`set-permissions`](#link7365742d7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f), [`when-permission`](#link7768656e2d7065726d697373696f6e), [`sys`](#link737973).	 [→index](#idx)

### `pop-error-handler` : procedure/0

Usage: `(pop-error-handler) => proc`

Remove the topmost error handler from the error handler stack and return it. For internal use only.

See also: [`with-error-handler`](#link776974682d6572726f722d68616e646c6572).	 [→index](#idx)

### `pop-finalizer` : procedure/0

Usage: `(pop-finalizer) => proc`

Remove a finalizer from the finalizer stack and return it. For internal use only.

See also: [`push-finalizer`](#link707573682d66696e616c697a6572), [`with-final`](#link776974682d66696e616c).	 [→index](#idx)

### `proc?` : macro/1

Usage: `(proc? arg) => bool`

Return true if `arg` is a procedure, nil otherwise.

See also: [`functional?`](#link66756e6374696f6e616c3f), [`closure?`](#link636c6f737572653f), [`functional-arity`](#link66756e6374696f6e616c2d6172697479), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx)

### `protect` : procedure/0 or more

Usage: `(protect [sym] ...)`

Protect symbols `sym` ... against changes or rebinding. The symbols need to be quoted. This operation requires the permission 'allow-protect to be set.

See also: [`protected?`](#link70726f7465637465643f), [`unprotect`](#link756e70726f74656374), [`dict-protect`](#link646963742d70726f74656374), [`dict-unprotect`](#link646963742d756e70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f), [`setq`](#link73657471), [`bind`](#link62696e64), [`interpret`](#link696e74657270726574).	 [→index](#idx)

### `protect-toplevel-symbols` : procedure/0

Usage: `(protect-toplevel-symbols)`

Protect all toplevel symbols that are not yet protected and aren't in the *mutable-toplevel-symbols* dict.

See also: [`protected?`](#link70726f7465637465643f), [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`declare-unprotected`](#link6465636c6172652d756e70726f746563746564), [`declare-volatile`](#link6465636c6172652d766f6c6174696c65), [`when-permission?`](#link7768656e2d7065726d697373696f6e3f), [`dict-protect`](#link646963742d70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`dict-unprotect`](#link646963742d756e70726f74656374).	 [→index](#idx)

### `protected?` : procedure/1

Usage: `(protected? sym)`

Return true if `sym` is protected, nil otherwise.

See also: [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`dict-unprotect`](#link646963742d756e70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`permission`](#link7065726d697373696f6e), [`permission?`](#link7065726d697373696f6e3f), [`setq`](#link73657471), [`bind`](#link62696e64), [`interpret`](#link696e74657270726574).	 [→index](#idx)

### `push-error-handler` : procedure/1

Usage: `(push-error-handler proc)`

Push an error handler `proc` on the error handler stack. For internal use only.

See also: [`with-error-handler`](#link776974682d6572726f722d68616e646c6572).	 [→index](#idx)

### `push-finalizer` : procedure/1

Usage: `(push-finalizer proc)`

Push a finalizer procedure `proc` on the finalizer stack. For internal use only.

See also: [`with-final`](#link776974682d66696e616c), [`pop-finalizer`](#link706f702d66696e616c697a6572).	 [→index](#idx)

### `read-eval-reply` : procedure/0

Usage: `(read-eval-reply)`

Start a new read-eval-reply loop.

See also: [`end-input`](#link656e642d696e707574), [`sys`](#link737973).	 [→index](#idx)

**Warning: Internal use only. This function might not do what you expect it to do.**

### `remove-hook` : procedure/2

Usage: `(remove-hook hook id) => bool`

Remove the symbolic or numberic `hook` with `id` and return true if the hook was removed, nil otherwise.

See also: [`add-hook`](#link6164642d686f6f6b), [`remove-hooks`](#link72656d6f76652d686f6f6b73), [`replace-hook`](#link7265706c6163652d686f6f6b).	 [→index](#idx)

### `remove-hook-internal` : procedure/2

Usage: `(remove-hook-internal hook id)`

Remove the hook with ID `id` from numeric `hook.`

See also: [`remove-hook`](#link72656d6f76652d686f6f6b).	 [→index](#idx)

**Warning: Internal use only.**

### `remove-hooks` : procedure/1

Usage: `(remove-hooks hook) => bool`

Remove all hooks for symbolic or numeric `hook`, return true if the hook exists and the associated procedures were removed, nil otherwise.

See also: [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b), [`replace-hook`](#link7265706c6163652d686f6f6b).	 [→index](#idx)

### `replace-hook` : procedure/2

Usage: `(replace-hook hook proc)`

Remove all hooks for symbolic or numeric `hook` and install the given `proc` as the only hook procedure.

See also: [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b), [`remove-hooks`](#link72656d6f76652d686f6f6b73).	 [→index](#idx)

### `run-hook` : procedure/1

Usage: `(run-hook hook)`

Manually run the hook, executing all procedures for the hook.

See also: [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b).	 [→index](#idx)

### `run-hook-internal` : procedure/1 or more

Usage: `(run-hook-internal hook [args] ...)`

Run all hooks for numeric hook ID `hook` with `args`... as arguments.

See also: [`run-hook`](#link72756e2d686f6f6b).	 [→index](#idx)

**Warning: Internal use only.**

### `run-selftest` : procedure/0

Usage: `(run-selftest)`

Run a self test of the Z3S5 Lisp system and report errors to standard output.

See also: [`help`](#link68656c70), [`testing`](#link74657374696e67).	 [→index](#idx)

### `sanity-check-help-system` : procedure/0

Usage: `(sanity-check-help-system)`

Display information about the help system such as missing entries, unnecessary help entries, and faulty help entries.

See also: [`find-missing-help-entries`](#link66696e642d6d697373696e672d68656c702d656e7472696573), [`find-unneeded-help-entries`](#link66696e642d756e6e65656465642d68656c702d656e7472696573), [`find-help-entries-with-incorrect-arity`](#link66696e642d68656c702d656e74726965732d776974682d696e636f72726563742d6172697479).	 [→index](#idx)

### set-permissions : nil

Usage: `(set-permissions li)`

Set the permissions for the current interpreter. This will trigger an error when the permission cannot be set due to a security violation. Generally, permissions can only be downgraded (made more stringent) and never relaxed. See the information for `permissions` for an overview of symbolic flags.

See also: [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f), [`when-permission`](#link7768656e2d7065726d697373696f6e), [`sys`](#link737973).	 [→index](#idx)

### `sleep` : procedure/1

Usage: `(sleep ms)`

Halt the current task execution for `ms` milliseconds.

See also: [`sleep-ns`](#link736c6565702d6e73), [`time`](#link74696d65), [`now`](#link6e6f77), [`now-ns`](#link6e6f772d6e73).	 [→index](#idx)

### `sleep-ns` : procedure/1

Usage: `(sleep-ns n`

Halt the current task execution for `n` nanoseconds.

See also: [`sleep`](#link736c656570), [`time`](#link74696d65), [`now`](#link6e6f77), [`now-ns`](#link6e6f772d6e73).	 [→index](#idx)

### `sys-key?` : procedure/1

Usage: `(sys-key? key) => bool`

Return true if the given sys key `key` exists, nil otherwise.

See also: [`sys`](#link737973), [`setsys`](#link736574737973).	 [→index](#idx)

### `sysmsg` : procedure/1

Usage: `(sysmsg msg)`

Asynchronously display a system message string `msg` if in console or page mode, otherwise the message is logged.

See also: [`sysmsg*`](#link7379736d73672a), [`synout`](#link73796e6f7574), [`synouty`](#link73796e6f757479), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx)

### `sysmsg*` : procedure/1

Usage: `(sysmsg* msg)`

Display a system message string `msg` if in console or page mode, otherwise the message is logged.

See also: [`sysmsg`](#link7379736d7367), [`synout`](#link73796e6f7574), [`synouty`](#link73796e6f757479), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx)

### `testing` : macro/1

Usage: `(testing name)`

Registers the string `name` as the name of the tests that are next registered with expect.

See also: [`expect`](#link657870656374), [`expect-err`](#link6578706563742d657272), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374).	 [→index](#idx)

### `try` : macro/2 or more

Usage: `(try (finals ...) body ...)`

Evaluate the forms of the `body` and afterwards the forms in `finals`. If during the execution of `body` an error occurs, first all `finals` are executed and then the error is printed by the default error printer.

See also: [`with-final`](#link776974682d66696e616c), [`with-error-handler`](#link776974682d6572726f722d68616e646c6572).	 [→index](#idx)

### `unprotect` : procedure/0 or more

Usage: `(unprotect [sym] ...)`

Unprotect symbols `sym` ..., allowing mutation or rebinding them. The symbols need to be quoted. This operation requires the permission 'allow-unprotect to be set, or else an error is caused.

See also: [`protect`](#link70726f74656374), [`protected?`](#link70726f7465637465643f), [`dict-unprotect`](#link646963742d756e70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f), [`setq`](#link73657471), [`bind`](#link62696e64), [`interpret`](#link696e74657270726574).	 [→index](#idx)

### `unprotect-toplevel-symbols` : procedure/0

Usage: `(unprotect-toplevel-symbols)`

Attempts to unprotect all toplevel symbols.

See also: [`protect-toplevel-symbols`](#link70726f746563742d746f706c6576656c2d73796d626f6c73), [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`declare-unprotected`](#link6465636c6172652d756e70726f746563746564).	 [→index](#idx)

### `warn` : procedure/1 or more

Usage: `(warn msg [args...])`

Output the warning message `msg` in error colors. The optional `args` are applied to the message as in fmt. The message should not end with a newline.

See also: [`error`](#link6572726f72).	 [→index](#idx)

### `when-permission` : macro/1 or more

Usage: `(when-permission perm body ...) => any`

Execute the expressions in `body` if and only if the symbolic permission `perm` is available.

See also: [`permission?`](#link7065726d697373696f6e3f).	 [→index](#idx)

### `with-colors` : procedure/3

Usage: `(with-colors textcolor backcolor proc)`

Execute `proc` for display side effects, where the default colors are set to `textcolor` and `backcolor`. These are color specifications like in the-color. After `proc` has finished or if an error occurs, the default colors are restored to their original state.

See also: [`the-color`](#link7468652d636f6c6f72), [`color`](#link636f6c6f72), [`set-color`](#link7365742d636f6c6f72), [`with-final`](#link776974682d66696e616c).	 [→index](#idx)

### `with-error-handler` : macro/2 or more

Usage: `(with-error-handler handler body ...)`

Evaluate the forms of the `body` with error handler `handler` in place. The handler is a procedure that takes the error as argument and handles it. If an error occurs in `handler`, a default error handler is used. Handlers are only active within the same thread.

See also: [`with-final`](#link776974682d66696e616c).	 [→index](#idx)

### `with-final` : macro/2 or more

Usage: `(with-final finalizer body ...)`

Evaluate the forms of the `body` with the given finalizer as error handler. If an error occurs, then `finalizer` is called with that error and nil. If no error occurs, `finalizer` is called with nil as first argument and the result of evaluating all forms of `body` as second argument.

See also: [`with-error-handler`](#link776974682d6572726f722d68616e646c6572).	 [→index](#idx)











## Time & Date {#time}

This section lists functions that are time and date-related. Most of them use `(now)` and turn it into more human-readable form.

### `date->epoch-ns` : procedure/7

Usage: `(date->epoch-ns Y M D h m s ns) => int`

Return the Unix epoch nanoseconds based on the given year `Y`, month `M`, day `D`, hour `h`, minute `m`, seconds `s`, and nanosecond fraction of a second `ns`, as it is e.g. returned in a (now) datelist.

See also: [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`datestr`](#link64617465737472), [`datestr*`](#link646174657374722a), [`day-of-week`](#link6461792d6f662d7765656b), [`week-of-date`](#link7765656b2d6f662d64617465), [`now`](#link6e6f77).	 [→index](#idx)

### `datelist->epoch-ns` : procedure/1

Usage: `(datelist->epoch-ns dateli) => int`

Convert a datelist to Unix epoch nanoseconds. This function uses the Unix nanoseconds from the 5th value of the second list in the datelist, as it is provided by functions like (now). However, if the Unix nanoseconds value is not specified in the list, it uses `date->epoch-ns` to convert to Unix epoch nanoseconds. Datelists can be incomplete. If the month is not specified, January is assumed. If the day is not specified, the 1st is assumed. If the hour is not specified, 12 is assumed, and corresponding defaults for minutes, seconds, and nanoseconds are 0.

See also: [`date->epoch-ns`](#link646174652d3e65706f63682d6e73), [`datestr`](#link64617465737472), [`datestr*`](#link646174657374722a), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374), [`now`](#link6e6f77).	 [→index](#idx)

### `datestr` : procedure/1

Usage: `(datestr datelist) => str`

Return datelist, as it is e.g. returned by (now), as a string in format YYYY-MM-DD HH:mm.

See also: [`now`](#link6e6f77), [`datestr*`](#link646174657374722a), [`datestr->datelist`](#link646174657374722d3e646174656c697374).	 [→index](#idx)

### `datestr*` : procedure/1

Usage: `(datestr* datelist) => str`

Return the datelist, as it is e.g. returned by (now), as a string in format YYYY-MM-DD HH:mm:ss.nanoseconds.

See also: [`now`](#link6e6f77), [`datestr`](#link64617465737472), [`datestr->datelist`](#link646174657374722d3e646174656c697374).	 [→index](#idx)

### `datestr->datelist` : procedure/1

Usage: `(datestr->datelist s) => li`

Convert a date string in the format of datestr and datestr* into a date list as it is e.g. returned by (now).

See also: [`datestr*`](#link646174657374722a), [`datestr`](#link64617465737472), [`now`](#link6e6f77).	 [→index](#idx)

### `day+` : procedure/2

Usage: `(day+ dateli n) => dateli`

Adds `n` days to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`minute+`](#link6d696e7574652b), [`hour+`](#link686f75722b), [`week+`](#link7765656b2b), [`month+`](#link6d6f6e74682b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx)

### `day-of-week` : procedure/3

Usage: `(day-of-week Y M D) => int`

Return the day of week based on the date with year `Y`, month `M`, and day `D`. The first day number 0 is Sunday, the last day is Saturday with number 6.

See also: [`week-of-date`](#link7765656b2d6f662d64617465), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`date->epoch-ns`](#link646174652d3e65706f63682d6e73), [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374), [`datestr`](#link64617465737472), [`datestr*`](#link646174657374722a), [`now`](#link6e6f77).	 [→index](#idx)

### `epoch-ns->datelist` : procedure/1

Usage: `(epoch-ns->datelist ns) => li`

Return the date list in UTC time corresponding to the Unix epoch nanoseconds `ns.`

See also: [`date->epoch-ns`](#link646174652d3e65706f63682d6e73), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`datestr`](#link64617465737472), [`datestr*`](#link646174657374722a), [`day-of-week`](#link6461792d6f662d7765656b), [`week-of-date`](#link7765656b2d6f662d64617465), [`now`](#link6e6f77).	 [→index](#idx)

### `hour+` : procedure/2

Usage: `(hour+ dateli n) => dateli`

Adds `n` hours to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`minute+`](#link6d696e7574652b), [`day+`](#link6461792b), [`week+`](#link7765656b2b), [`month+`](#link6d6f6e74682b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx)

### `minute+` : procedure/2

Usage: `(minute+ dateli n) => dateli`

Adds `n` minutes to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`hour+`](#link686f75722b), [`day+`](#link6461792b), [`week+`](#link7765656b2b), [`month+`](#link6d6f6e74682b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx)

### `month+` : procedure/2

Usage: `(month+ dateli n) => dateli`

Adds `n` months to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`minute+`](#link6d696e7574652b), [`hour+`](#link686f75722b), [`day+`](#link6461792b), [`week+`](#link7765656b2b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx)

### `now` : procedure/0

Usage: `(now) => li`

Return the current datetime in UTC format as a list of values in the form '((year month day weekday iso-week) (hour minute second nanosecond unix-nano-second)).

See also: [`now-ns`](#link6e6f772d6e73), [`datestr`](#link64617465737472), [`time`](#link74696d65), [`date->epoch-ns`](#link646174652d3e65706f63682d6e73), [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374).	 [→index](#idx)

### `now-ms` : procedure/0

Usage: `(now-ms) => num`

Return the relative system time as a call to (now-ns) but in milliseconds.

See also: [`now-ns`](#link6e6f772d6e73), [`now`](#link6e6f77).	 [→index](#idx)

### `now-ns` : procedure/0

Usage: `(now-ns) => int`

Return the current time in Unix nanoseconds.

See also: [`now`](#link6e6f77), [`time`](#link74696d65).	 [→index](#idx)

### `sec+` : procedure/2

Usage: `(sec+ dateli n) => dateli`

Adds `n` seconds to the given date `dateli` in datelist format and returns the new datelist.

See also: [`minute+`](#link6d696e7574652b), [`hour+`](#link686f75722b), [`day+`](#link6461792b), [`week+`](#link7765656b2b), [`month+`](#link6d6f6e74682b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx)

### `time` : procedure/1

Usage: `(time proc) => int`

Return the time in nanoseconds that it takes to execute the procedure with no arguments `proc.`

See also: [`now-ns`](#link6e6f772d6e73), [`now`](#link6e6f77).	 [→index](#idx)

### `week+` : procedure/2

Usage: `(week+ dateli n) => dateli`

Adds `n` weeks to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`minute+`](#link6d696e7574652b), [`hour+`](#link686f75722b), [`day+`](#link6461792b), [`month+`](#link6d6f6e74682b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx)

### `week-of-date` : procedure/3

Usage: `(week-of-date Y M D) => int`

Return the week of the date in the year given by year `Y`, month `M`, and day `D.`

See also: [`day-of-week`](#link6461792d6f662d7765656b), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`date->epoch-ns`](#link646174652d3e65706f63682d6e73), [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374), [`datestr`](#link64617465737472), [`datestr*`](#link646174657374722a), [`now`](#link6e6f77).	 [→index](#idx)

### `year+` : procedure/2

Usage: `(month+ dateli n) => dateli`

Adds `n` years to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`minute+`](#link6d696e7574652b), [`hour+`](#link686f75722b), [`day+`](#link6461792b), [`week+`](#link7765656b2b), [`month+`](#link6d6f6e74682b), [`now`](#link6e6f77).	 [→index](#idx)





## User Interface {#ui}

This section lists miscellaneous user interface commands such as color for terminals.

### *colors* : dict

Usage: `*colors*`

A global dict that maps default color names to color lists (r g b), (r g b a) or selectors for (color selector). This can be used with procedure the-color to translate symbolic names to colors.

See also: [`the-color`](#link7468652d636f6c6f72).	 [→index](#idx)

### `color` : procedure/1

Usage: `(color sel) => (r g b a)`

Return the color based on `sel`, which may be 'text for the text color, 'back for the background color, 'textarea for the color of the text area, 'gfx for the current graphics foreground color, and 'frame for the frame color. In standard Z3S5 Lisp only 'text and 'back are available as selectors and implementations are free to ignore these.

See also: [`set-color`](#link7365742d636f6c6f72), [`reset-color`](#link72657365742d636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`with-colors`](#link776974682d636f6c6f7273).	 [→index](#idx)

### `darken` : procedure/1

Usage: `(darken color [amount]) => (r g b a)`

Return a darker version of `color`. The optional positive `amount` specifies the amount of darkening (0-255).

See also: [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a), [`lighten`](#link6c69676874656e).	 [→index](#idx)

### `lighten` : procedure/1

Usage: `(lighten color [amount]) => (r g b a)`

Return a lighter version of `color`. The optional positive `amount` specifies the amount of lightening (0-255).

See also: [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a), [`darken`](#link6461726b656e).	 [→index](#idx)

### `out` : procedure/1

Usage: `(out expr)`

Output `expr` on the console with current default background and foreground color.

See also: [`outy`](#link6f757479), [`synout`](#link73796e6f7574), [`synouty`](#link73796e6f757479), [`output-at`](#link6f75747075742d6174).	 [→index](#idx)

### `outy` : procedure/1

Usage: `(outy spec)`

Output styled text specified in `spec`. A specification is a list of lists starting with 'fg for foreground, 'bg for background, or 'text for unstyled text. If the list starts with 'fg or 'bg then the next element must be a color suitable for (the-color spec). Following may be a string to print or another color specification. If a list starts with 'text then one or more strings may follow.

See also: [`*colors*`](#link2a636f6c6f72732a), [`the-color`](#link7468652d636f6c6f72), [`set-color`](#link7365742d636f6c6f72), [`color`](#link636f6c6f72), [`gfx.color`](#link6766782e636f6c6f72), [`output-at`](#link6f75747075742d6174), [`out`](#link6f7574).	 [→index](#idx)

### `random-color` : procedure/0 or more

Usage: `(random-color [alpha])`

Return a random color with optional `alpha` value. If `alpha` is not specified, it is 255.

See also: [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a), [`darken`](#link6461726b656e), [`lighten`](#link6c69676874656e).	 [→index](#idx)

### `reset-color` : procedure/0

Usage: `(reset-color)`

Reset the 'text and 'back colors of the display to default values. These values are not specified in the color database and depend on the runtime implementation. Other colors like 'gfx or 'frame are not affected.

See also: [`set-color`](#link7365742d636f6c6f72), [`color`](#link636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`with-colors`](#link776974682d636f6c6f7273).	 [→index](#idx)

### `set-color` : procedure/1

Usage: `(set-color sel colorlist)`

Set the color according to `sel` to the color `colorlist` of the form '(r g b a). See `color` for information about `sel.`

See also: [`color`](#link636f6c6f72), [`reset-color`](#link72657365742d636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`with-colors`](#link776974682d636f6c6f7273).	 [→index](#idx)

### `synout` : procedure/1

Usage: `(synout arg)`

Like out, but enforcing a new input line afterwards. This needs to be used when outputing concurrently in a future or task.

See also: [`out`](#link6f7574), [`outy`](#link6f757479), [`synouty`](#link73796e6f757479).	 [→index](#idx)

**Warning: Concurrent display output can lead to unexpected visual results and ought to be avoided.**

### `the-color` : procedure/1

Usage: `(the-color colors-spec) => (r g b a)`

Return the color list (r g b a) based on a color specification, which may be a color list (r g b), a color selector for (color selector) or a color name such as 'dark-blue.

See also: [`*colors*`](#link2a636f6c6f72732a), [`color`](#link636f6c6f72), [`set-color`](#link7365742d636f6c6f72), [`outy`](#link6f757479).	 [→index](#idx)

### `the-color-names` : procedure/0

Usage: `(the-color-names) => li`

Return the list of color names in *colors*.

See also: [`*colors*`](#link2a636f6c6f72732a), [`the-color`](#link7468652d636f6c6f72).	 [→index](#idx)



## Unicode Standard {#unicode}

This section lists helper function to deal with the unicode standard. Strings in Z3S5 Lisp can hold arbitrary data but for I/O purposes are supposed to be in UTF-8 format. Unicode helper functions can take a unicode rune as a string, in case of which only the first rune of the string is considered and the empty yields an error, or as a number.

### `unicode.is-control?` : procedure/1

Usage: `(unicode.is-control? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a control character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-digit?` : procedure/1

Usage: `(unicode.is-digit? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a numerical digit, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-graphic?` : procedure/1

Usage: `(unicode.is-graphic? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` is graphically visible, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-letter?` : procedure/1

Usage: `(unicode.is-letter? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a letter, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-lower?` : procedure/1

Usage: `(unicode.is-lower? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` is a lowercase character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-mark?` : procedure/1

Usage: `(unicode.is-mark? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a mark (unicode category M), nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-number?` : procedure/1

Usage: `(unicode.is-number? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a letter that may occur in a number, nil otherwise. This is for testing the unicode number category, which contains number and digit glyphs in other writing system. Note that it does not test whether a character belongs to a valid Z3S5 Lisp number.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-print?` : procedure/1

Usage: `(unicode.is-print? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a printable character.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f).	 [→index](#idx)

### `unicode.is-punct?` : procedure/1

Usage: `(unicode.is-punct? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a punctuation character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-space?` : procedure/1

Usage: `(unicode.is-space? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a white space character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-symbol?` : procedure/1

Usage: `(unicode.is-symbol? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a symbol, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-title?` : procedure/1

Usage: `(unicode.is-title? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a titlecase character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)

### `unicode.is-upper?` : procedure/1

Usage: `(unicode.is-upper? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents an uppercase character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx)









## Runtime System Images {#zimage}

The following functions provide functionality for saving, loading, and running of runtime system images to and from disk.

### `current-zimage` : procedure/0

Usage: `(current-zimage [nonce]) => dict`

Obtain a dict of all toplevel bindings. If the `nonce` is provided, procedures are externalized as (nonce proc) to distinguish them from data. This function may use a lot of memory. Consider saving or loading zimages directly from disk instead. Notice that the dict is not the same format as the one used by load-zimage and save-zimage.

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`save-zimage`](#link736176652d7a696d616765), [`externalize`](#link65787465726e616c697a65).	 [→index](#idx)

### `declare-volatile` : procedure/1

Usage: `(declare-volatile sym)`

Declares `sym`, which has to be quoted, as a volatile toplevel symbol. Volatile toplevel symbols are neither saved to nor loaded from zimages.

See also: [`save-zimage`](#link736176652d7a696d616765), [`load-zimage`](#link6c6f61642d7a696d616765), [`declare-unprotected`](#link6465636c6172652d756e70726f746563746564).	 [→index](#idx)

### `load-zimage` : procedure/1 or more

Usage: `(load-zimage fi) => li`

Load the zimage file `fi`, if possible, and return a list containing information about the zimage after it has been loaded. If the zimage fails the semantic version check, then an error is raised.

See also: [`save-zimage`](#link736176652d7a696d616765), [`run-zimage`](#link72756e2d7a696d616765), [`zimage-loadable?`](#link7a696d6167652d6c6f616461626c653f).	 [→index](#idx)

### `read-zimage` : procedure/2

Usage: `(read-zimage in fi)`

Reads and evaluates the zimage in stream `in` from file `fi`. The file `fi` argument is used in error messages. This procedure raises errors when the zimage is malformed or the version check fails.

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`run-zimage`](#link72756e2d7a696d616765), [`zimage-header`](#link7a696d6167652d686561646572).	 [→index](#idx)

### `run-zimage` : procedure/1 or more

Usage: `(run-zimage fi)`

Load the zimage file `fi` and start it at the designated entry point. Raises an error if the zimage version is not compatible or the zimage cannot be run.

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`save-zimage`](#link736176652d7a696d616765), [`zimage-runable?`](#link7a696d6167652d72756e61626c653f), [`zimage-loadable?`](#link7a696d6167652d6c6f616461626c653f).	 [→index](#idx)

### `save-zimage` : procedure/1 or more

Usage: `(save-zimage min-version info entry-point fi) => int`

Write the current state of the system as a zimage to file `fi`. If the file already exists, it is overwritten. The `min-version` argument designates the minimum system version required to load the zimage. The `info` argument should be a list whose first argument is a human-readable string explaining the purpose of the zimage and remainder is user data. The `entry-point` is either nil or an expression that can be evaluated to start the zimage after it has been loaded with run-zimage.

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`current-zimage`](#link63757272656e742d7a696d616765), [`dump`](#link64756d70), [`run-zimage`](#link72756e2d7a696d616765), [`zimage-loadable?`](#link7a696d6167652d6c6f616461626c653f), [`zimage-runable?`](#link7a696d6167652d72756e61626c653f), [`externalize`](#link65787465726e616c697a65).	 [→index](#idx)

### `write-zimage` : procedure/4

Usage: `(write-zimage out min-version info entry-point) => list`

Write the current state of the system as an zimage to stream `out`. The `min-version` argument designates the minimum system version required to load the zimage. The `info` argument should be a list whose first argument is a human-readable string explaining the purpose of the zimage and remainder is user data. The `entry-point` is either nil or an expression that can be evaluated to start the zimage after it has been loaded with run-zimage. The procedure returns a header with information of the zimage.

See also: [`save-zimage`](#link736176652d7a696d616765), [`read-zimage`](#link726561642d7a696d616765), [`load-zimage`](#link6c6f61642d7a696d616765), [`current-zimage`](#link63757272656e742d7a696d616765), [`externalize`](#link65787465726e616c697a65).	 [→index](#idx)

### `zimage-header` : procedure/1

Usage: `(zimage-header fi) => li`

Return the zimage header from file `fi.`

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`run-zimage`](#link72756e2d7a696d616765).	 [→index](#idx)

### `zimage-loadable?` : procedure/1 or more

Usage: `(zimage-loadable? fi)`

Checks whether the file `fi` is loadable. This does not check whether the file actually is an zimage file, so you can only use this on readable lisp files.

See also: [`zimage-runable?`](#link7a696d6167652d72756e61626c653f), [`load-zimage`](#link6c6f61642d7a696d616765), [`save-zimage`](#link736176652d7a696d616765), [`current-zimage`](#link63757272656e742d7a696d616765).	 [→index](#idx)

### `zimage-runable?` : procedure/1 or more

Usage: `(zimage-runable? [sel] fi`

Returns the non-nil entry-point of the zimage if the the zimage in file `fi` can be run, nil otherwise.

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`zimage-loadable?`](#link7a696d6167652d6c6f616461626c653f), [`save-zimage`](#link736176652d7a696d616765), [`current-zimage`](#link63757272656e742d7a696d616765).	 [→index](#idx)



# Complete Reference {#reference}

## `%` : procedure/2 {#link25}

Usage: `(% x y) => num`

Compute the remainder of dividing number `x` by `y.`

See also: [`mod`](#link6d6f64), [`/`](#link2f).	 [→index](#idx) [→topic](#numeric)

## `*` : procedure/0 or more {#link2a}

Usage: `(* [args] ...) => num`

Multiply all `args`. Special cases: (*) is 1 and (* x) is x.

See also: [`+`](#link2b), [`-`](#link2d), [`/`](#link2f).	 [→index](#idx) [→topic](#numeric)

## *colors* : dict {#link2a636f6c6f72732a}

Usage: `*colors*`

A global dict that maps default color names to color lists (r g b), (r g b a) or selectors for (color selector). This can be used with procedure the-color to translate symbolic names to colors.

See also: [`the-color`](#link7468652d636f6c6f72).	 [→index](#idx) [→topic](#ui)

## *error-handler* : dict {#link2a6572726f722d68616e646c65722a}

Usage: `(*error-handler* err)`

The global error handler dict that contains procedures which take an error and handle it. If an entry is nil, the default handler is used, which outputs the error using *error-printer*. The dict contains handlers based on concurrent thread IDs and ought not be manipulated directly.

See also: [`*error-printer*`](#link2a6572726f722d7072696e7465722a).	 [→index](#idx) [→topic](#system)

## `*error-printer*` : procedure/1 {#link2a6572726f722d7072696e7465722a}

Usage: `(*error-printer* err)`

The global printer procedure which takes an error and prints it.

See also: [`error`](#link6572726f72).	 [→index](#idx) [→topic](#system)

## *help* : dict {#link2a68656c702a}

Usage: `*help*`

Dict containing all help information for symbols.

See also: [`help`](#link68656c70), [`defhelp`](#link64656668656c70), [`apropos`](#link6170726f706f73).	 [→index](#idx) [→topic](#help)

## *hooks* : dict {#link2a686f6f6b732a}

Usage: `*hooks*`

A dict containing translations from symbolic names to the internal numeric representations of hooks.

See also: [`hook`](#link686f6f6b), [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b), [`remove-hooks`](#link72656d6f76652d686f6f6b73).	 [→index](#idx)

## *last-error* : sym {#link2a6c6173742d6572726f722a}

Usage: `*last-error* => str`

Contains the last error that has occurred.

See also: [`*error-printer*`](#link2a6572726f722d7072696e7465722a), [`*error-handler*`](#link2a6572726f722d68616e646c65722a).	 [→index](#idx)

**Warning: This may only be used for debugging! Do *not* use this for error handling, it will surely fail!** [→topic](#system)

## *reflect* : symbol {#link2a7265666c6563742a}

Usage: `*reflect* => li`

The list of feature identifiers as symbols that this Lisp implementation supports.

See also: [`feature?`](#link666561747572653f), [`on-feature`](#link6f6e2d66656174757265).	 [→index](#idx) [→topic](#system)

## `+` : procedure/0 or more {#link2b}

Usage: `(+ [args] ...) => num`

Sum up all `args`. Special cases: (+) is 0 and (+ x) is x.

See also: [`-`](#link2d), [`*`](#link2a), [`/`](#link2f).	 [→index](#idx) [→topic](#numeric)

## `-` : procedure/1 or more {#link2d}

Usage: `(- x [y1] [y2] ...) => num`

Subtract `y1`, `y2`, ..., from `x`. Special case: (- x) is -x.

See also: [`+`](#link2b), [`*`](#link2a), [`/`](#link2f).	 [→index](#idx) [→topic](#numeric)

## `/` : procedure/1 or more {#link2f}

Usage: `(/ x y1 [y2] ...) => float`

Divide `x` by `y1`, then by `y2`, and so forth. The result is a float.

See also: [`+`](#link2b), [`*`](#link2a), [`-`](#link2d).	 [→index](#idx) [→topic](#numeric)

## `/=` : procedure/2 {#link2f3d}

Usage: `(/= x y) => bool`

Return true if number `x` is not equal to `y`, nil otherwise.

See also: [`>`](#link3e), [`>=`](#link3e3d), [`<`](#link3c), [`<=`](#link3c3d).	 [→index](#idx) [→topic](#numeric)

## `10th` : procedure/1 or more {#link31307468}

Usage: `(10th seq [default]) => any`

Get the tenth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468).	 [→index](#idx) [→topic](#seq)

## `1st` : procedure/1 or more {#link317374}

Usage: `(1st seq [default]) => any`

Get the first element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `2nd` : procedure/1 or more {#link326e64}

Usage: `(2nd seq [default]) => any`

Get the second element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `3rd` : procedure/1 or more {#link337264}

Usage: `(3rd seq [default]) => any`

Get the third element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `4th` : procedure/1 or more {#link347468}

Usage: `(4th seq [default]) => any`

Get the fourth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `5th` : procedure/1 or more {#link357468}

Usage: `(5th seq [default]) => any`

Get the fifth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `6th` : procedure/1 or more {#link367468}

Usage: `(6th seq [default]) => any`

Get the sixth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `7th` : procedure/1 or more {#link377468}

Usage: `(7th seq [default]) => any`

Get the seventh element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `8th` : procedure/1 or more {#link387468}

Usage: `(8th seq [default]) => any`

Get the eighth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `9th` : procedure/1 or more {#link397468}

Usage: `(9th seq [default]) => any`

Get the nineth element of a sequence or the optional `default`. If there is no such element and no default is provided, then an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string-ref`](#link737472696e672d726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `<` : procedure/2 {#link3c}

Usage: `(< x y) => bool`

Return true if `x` is smaller than `y.`

See also: [`<=`](#link3c3d), [`>=`](#link3e3d), [`>`](#link3e).	 [→index](#idx) [→topic](#numeric)

## `<=` : procedure/2 {#link3c3d}

Usage: `(<= x y) => bool`

Return true if `x` is smaller than or equal to `y`, nil otherwise.

See also: [`>`](#link3e), [`<`](#link3c), [`>=`](#link3e3d), [`/=`](#link2f3d).	 [→index](#idx) [→topic](#numeric)

## `=` : procedure/2 {#link3d}

Usage: `(= x y) => bool`

Return true if number `x` equals number `y`, nil otherwise.

See also: [`eql?`](#link65716c3f), [`equal?`](#link657175616c3f).	 [→index](#idx) [→topic](#numeric)

## `>` : procedure/2 {#link3e}

Usage: `(> x y) => bool`

Return true if `x` is larger than `y`, nil otherwise.

See also: [`<`](#link3c), [`>=`](#link3e3d), [`<=`](#link3c3d), [`/=`](#link2f3d).	 [→index](#idx) [→topic](#numeric)

## `>=` : procedure/2 {#link3e3d}

Usage: `(>= x y) => bool`

Return true if `x` is larger than or equal to `y`, nil otherwise.

See also: [`>`](#link3e), [`<`](#link3c), [`<=`](#link3c3d), [`/=`](#link2f3d).	 [→index](#idx) [→topic](#numeric)

## `abs` : procedure/1 {#link616273}

Usage: `(abs x) => num`

Returns the absolute value of number `x.`

See also: [`*`](#link2a), [`-`](#link2d), [`+`](#link2b), [`/`](#link2f).	 [→index](#idx) [→topic](#numeric)

## action : class {#link616374696f6e}

Usage: `(new action <info-clause> <name-clause> <proc-clause> ...) => action`

The action class describes instances of actions that serve as plugins for the system hosting Z3S5 Lisp. Each action has a `name`, `prefix` and `info` string property and a unique `id`. Property `args` is an array that specifies the type of arguments of the action. This may be used by an implementation of action.get-args. The `proc` property must be a function taking the action and a task-id as argument and processing the action sequentially until it is completed or `task-recv` returns the 'stop signal. An action may store the result of computation in the `result` property, an error in the `error` property, and an arbitrary state in the `state` property. After processing or if an error occurs, action.result should be called so the host can process the result or error. The action system requires the implementation of procedures action.start, action.progress, action.get-args, and action.result. These are usually defined in the host system, for example in the Go implementation of an application using Z3S5 Lisp actions, and serve as callback functions from Lisp to Go.

See also: [`action`](#link616374696f6e), [`action-stop`](#link616374696f6e2d73746f70), [`action.start`](#link616374696f6e2e7374617274), [`action.progress`](#link616374696f6e2e70726f6772657373), [`action.get-args`](#link616374696f6e2e6765742d61726773), [`action.result`](#link616374696f6e2e726573756c74).	 [→index](#idx) [→topic](#action)

## action-start : method {#link616374696f6e2d7374617274}

Usage: `(action-start action)`

Start `action`, which runs the action's `proc` in a task with the action and a task-id as argument. The `proc` of the `action` should periodically check for the 'stop signal using `task-recv` if the action should be cancellable, should call action.progress to report progress, action.error in case of an error, and action.result to report the result.

See also: [`action`](#link616374696f6e), [`action-stop`](#link616374696f6e2d73746f70), [`action-start`](#link616374696f6e2d7374617274), [`action.progress`](#link616374696f6e2e70726f6772657373), [`action.get-args`](#link616374696f6e2e6765742d61726773), [`action.result`](#link616374696f6e2e726573756c74).	 [→index](#idx) [→topic](#action)

## action-stop : method {#link616374696f6e2d73746f70}

Usage: `(action-stop action)`

The stop method sends a 'stop signal to the action's running `proc`. It is up to the `proc` to check for the signal using `task-recv` and terminate the action gracefully.

See also: [`action`](#link616374696f6e), [`action-stop`](#link616374696f6e2d73746f70), [`action-start`](#link616374696f6e2d7374617274), [`action.progress`](#link616374696f6e2e70726f6772657373), [`action.get-args`](#link616374696f6e2e6765742d61726773), [`action.result`](#link616374696f6e2e726573756c74).	 [→index](#idx) [→topic](#action)

## `activate-menu-last-submenu` : procedure/1 {#link61637469766174652d6d656e752d6c6173742d7375626d656e75}

Usage: `(activate-menu-last-submenu menu) => bool`

Find the last active menu item traversing through open submenus, and activate its submenu if one is found. Return true if a submenu was activated, nil otherwise.

See also: [`deactivate-menu-last-submenu`](#link646561637469766174652d6d656e752d6c6173742d7375626d656e75), [`new-menu`](#link6e65772d6d656e75), [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874), [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573).	 [→index](#idx) [→topic](#gui)

## `activate-menu-next` : procedure/1 {#link61637469766174652d6d656e752d6e657874}

Usage: `(activate-menu-next menu)`

Activate the menu item following the currently active menu item, if there is any.

See also: [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573), [`new-menu`](#link6e65772d6d656e75), [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75).	 [→index](#idx) [→topic](#gui)

## `activate-menu-previous` : procedure/1 {#link61637469766174652d6d656e752d70726576696f7573}

Usage: `(activate-menu-previous menu)`

Activate the menu item before the currently active menu item, if there is any.

See also: [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874), [`new-menu`](#link6e65772d6d656e75), [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75).	 [→index](#idx) [→topic](#gui)

## `add-canvas-shortcut` : procedure/3 {#link6164642d63616e7661732d73686f7274637574}

Usage: `(add-canvas-shortcut canvas shortcut proc)`

Add the given `shortcut` to the given `canvas`, calling the handler `proc` when it is triggered. `shortcut` must be a list consisting of valid keyboard modifier symbols and a valid key symbol. `proc` must be a function that takes a shortcut as argument. If multiple non-modifier keys are present, only the last one is taken. However, multiple modifier keys are possible. Possible modifiers are symbols or corresponding strings in '(shift control alt suprt). Possible keys are in '(escape return tab backspace insert delete right left down up page-up page-down home end f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 enter 0 1 2 3 4 5 6 7 8 9 key0 key1 key2 key3 key4 key5 key6 key7 key8 key9 a b c d e f g h i j k l m n o p q r s t u v w x y z space tick comma minus period slash backslash left-bracket right-bracket semicolon equal asterisk plus back-tick) and their string variants.

See also: [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`remove-canvas-shortcut`](#link72656d6f76652d63616e7661732d73686f7274637574).	 [→index](#idx) [→topic](#gui)

## `add-hook` : procedure/2 {#link6164642d686f6f6b}

Usage: `(add-hook hook proc) => id`

Add hook procedure `proc` which takes a list of arguments as argument under symbolic or numeric `hook` and return an integer hook `id` for this hook. If `hook` is not known, nil is returned.

See also: [`remove-hook`](#link72656d6f76652d686f6f6b), [`remove-hooks`](#link72656d6f76652d686f6f6b73), [`replace-hook`](#link7265706c6163652d686f6f6b).	 [→index](#idx) [→topic](#system)

## `add-hook-internal` : procedure/2 {#link6164642d686f6f6b2d696e7465726e616c}

Usage: `(add-hook-internal hook proc) => int`

Add a procedure `proc` to hook with numeric ID `hook` and return this procedures hook ID. The function does not check whether the hook exists.

See also: [`add-hook`](#link6164642d686f6f6b).	 [→index](#idx)

**Warning: Internal use only.** [→topic](#system)

## `add-hook-once` : procedure/2 {#link6164642d686f6f6b2d6f6e6365}

Usage: `(add-hook-once hook proc) => id`

Add a hook procedure `proc` which takes a list of arguments under symbolic or numeric `hook` and return an integer hook `id`. If `hook` is not known, nil is returned.

See also: [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b), [`replace-hook`](#link7265706c6163652d686f6f6b).	 [→index](#idx) [→topic](#system)

## `add-lisp-editor` : procedure/1 {#link6164642d6c6973702d656469746f72}

Usage: `(add-lisp-editor win)`

Add a lisp editor to the given window. This includes automatic help and looking up functions.

See also: [`lisped`](#link6c6973706564).	 [→index](#idx) [→topic](#gui)

## `add1` : procedure/1 {#link61646431}

Usage: `(add1 n) => num`

Add 1 to number `n.`

See also: [`sub1`](#link73756231), [`+`](#link2b), [`-`](#link2d).	 [→index](#idx) [→topic](#numeric)

## `alist->dict` : procedure/1 {#link616c6973742d3e64696374}

Usage: `(alist->dict li) => dict`

Convert an association list `li` into a dictionary. Note that the value will be the cdr of each list element, not the second element, so you need to use an alist with proper pairs '(a . b) if you want b to be a single value.

See also: [`dict->alist`](#link646963742d3e616c697374), [`dict`](#link64696374), [`dict->list`](#link646963742d3e6c697374), [`list->dict`](#link6c6973742d3e64696374).	 [→index](#idx) [→topic](#conversion)

## `alist?` : procedure/1 {#link616c6973743f}

Usage: `(alist? li) => bool`

Return true if `li` is an association list, nil otherwise. This also works for a-lists where each element is a pair rather than a full list.

See also: [`assoc`](#link6173736f63).	 [→index](#idx) [→topic](#lisp)

## `and` : macro/0 or more {#link616e64}

Usage: `(and expr1 expr2 ...) => any`

Evaluate `expr1` and if it is not nil, then evaluate `expr2` and if it is not nil, evaluate the next expression, until all expressions have been evaluated. This is a shortcut logical and.

See also: [`or`](#link6f72).	 [→index](#idx) [→topic](#lisp)

## `append` : procedure/1 or more {#link617070656e64}

Usage: `(append li1 li2 ...) => li`

Concatenate the lists given as arguments.

See also: [`cons`](#link636f6e73).	 [→index](#idx) [→topic](#lisp)

## `append-form` : procedure/0 {#link617070656e642d666f726d}

Usage: `(append-form form str canvas-object)`

Append a new row to the bottom `form` consisting of a label `str` aligned with a `canvas-object`, which may be an entry, button, etc.

See also: [`new-form`](#link6e65772d666f726d).	 [→index](#idx) [→topic](#gui)

## `apply` : procedure/2 {#link6170706c79}

Usage: `(apply proc arg) => any`

Apply function `proc` to argument list `arg.`

See also: [`functional?`](#link66756e6374696f6e616c3f).	 [→index](#idx) [→topic](#lisp)

## `apropos` : procedure/1 {#link6170726f706f73}

Usage: `(apropos sym) => #li`

Get a list of procedures and symbols related to `sym` from the help system.

See also: [`defhelp`](#link64656668656c70), [`help-entry`](#link68656c702d656e747279), [`help`](#link68656c70), [`*help*`](#link2a68656c702a).	 [→index](#idx) [→topic](#help)

## `array` : procedure/0 or more {#link6172726179}

Usage: `(array [arg1] ...) => array`

Create an array containing the arguments given to it.

See also: [`array?`](#link61727261793f), [`build-array`](#link6275696c642d6172726179), [`array+`](#link61727261792b).	 [→index](#idx) [→topic](#array)

## `array+` : procedure/1 or more {#link61727261792b}

Usage: `(array+ array1 ...) => array`

Create a new array that results from concatenating the given arrays in order. This function does not mutate `array1.`

See also: [`array-ref`](#link61727261792d726566), [`array-len`](#link61727261792d6c656e), [`build-array`](#link6275696c642d6172726179), [`array-slice`](#link61727261792d736c696365), [`array`](#link6172726179), [`array-copy`](#link61727261792d636f7079).	 [→index](#idx) [→topic](#array)

## `array->list` : procedure/1 {#link61727261792d3e6c697374}

Usage: `(array->list arr) => li`

Convert array `arr` into a list.

See also: [`list->array`](#link6c6973742d3e6172726179), [`array`](#link6172726179).	 [→index](#idx) [→topic](#conversion)

## `array->str` : procedure/1 {#link61727261792d3e737472}

Usage: `(array-str arr) => s`

Convert an array of unicode glyphs as integer values into a string. If the given sequence is not a valid UTF-8 sequence, an error is thrown.

See also: [`str->array`](#link7374722d3e6172726179).	 [→index](#idx) [→topic](#conversion)

## `array-append` : procedure/2 {#link61727261792d617070656e64}

Usage: `(array-append arr elem) => array`

Append `elem` to the array `arr`. This function is destructive and mutates the array. Use array-copy if you need a copy.

See also: [`array-ref`](#link61727261792d726566), [`array-len`](#link61727261792d6c656e), [`build-array`](#link6275696c642d6172726179), [`array-slice`](#link61727261792d736c696365), [`array`](#link6172726179), [`array-copy`](#link61727261792d636f7079), [`array+`](#link61727261792b).	 [→index](#idx) [→topic](#array)

## `array-copy` : procedure/1 {#link61727261792d636f7079}

Usage: `(array-copy arr) => array`

Return a copy of `arr.`

See also: [`array`](#link6172726179), [`array?`](#link61727261793f), [`array-map!`](#link61727261792d6d617021), [`array-pmap!`](#link61727261792d706d617021).	 [→index](#idx) [→topic](#array)

## `array-exists?` : procedure/2 {#link61727261792d6578697374733f}

Usage: `(array-exists? arr pred) => bool`

Return true if `pred` returns true for at least one element in array `arr`, nil otherwise.

See also: [`exists?`](#link6578697374733f), [`forall?`](#link666f72616c6c3f), [`list-exists?`](#link6c6973742d6578697374733f), [`str-exists?`](#link7374722d6578697374733f), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#array)

## `array-forall?` : procedure/2 {#link61727261792d666f72616c6c3f}

Usage: `(array-forall? arr pred) => bool`

Return true if predicate `pred` returns true for all elements of array `arr`, nil otherwise.

See also: [`foreach`](#link666f7265616368), [`map`](#link6d6170), [`forall?`](#link666f72616c6c3f), [`str-forall?`](#link7374722d666f72616c6c3f), [`list-forall?`](#link6c6973742d666f72616c6c3f), [`exists?`](#link6578697374733f).	 [→index](#idx) [→topic](#array)

## `array-foreach` : procedure/2 {#link61727261792d666f7265616368}

Usage: `(array-foreach arr proc)`

Apply `proc` to each element of array `arr` in order, for the side effects.

See also: [`foreach`](#link666f7265616368), [`list-foreach`](#link6c6973742d666f7265616368), [`map`](#link6d6170).	 [→index](#idx) [→topic](#array)

## `array-len` : procedure/1 {#link61727261792d6c656e}

Usage: `(array-len arr) => int`

Return the length of array `arr.`

See also: [`len`](#link6c656e).	 [→index](#idx) [→topic](#array)

## `array-map!` : procedure/2 {#link61727261792d6d617021}

Usage: `(array-map! arr proc)`

Traverse array `arr` in unspecified order and apply `proc` to each element. This mutates the array.

See also: [`array-walk`](#link61727261792d77616c6b), [`array-pmap!`](#link61727261792d706d617021), [`array?`](#link61727261793f), [`map`](#link6d6170), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#array)

## `array-pmap!` : procedure/2 {#link61727261792d706d617021}

Usage: `(array-pmap! arr proc)`

Apply `proc` in unspecified order in parallel to array `arr`, mutating the array to contain the value returned by `proc` each time. Because of the calling overhead for parallel execution, for many workloads array-map! might be faster if `proc` is very fast. If `proc` is slow, then array-pmap! may be much faster for large arrays on machines with many cores.

See also: [`array-map!`](#link61727261792d6d617021), [`array-walk`](#link61727261792d77616c6b), [`array?`](#link61727261793f), [`map`](#link6d6170), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#array)

## `array-ref` : procedure/1 {#link61727261792d726566}

Usage: `(array-ref arr n) => any`

Return the element of `arr` at index `n`. Arrays are 0-indexed.

See also: [`array?`](#link61727261793f), [`array`](#link6172726179), [`nth`](#link6e7468), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#array)

## `array-reverse` : procedure/1 {#link61727261792d72657665727365}

Usage: `(array-reverse arr) => array`

Create a copy of `arr` that reverses the order of all of its elements.

See also: [`reverse`](#link72657665727365), [`list-reverse`](#link6c6973742d72657665727365), [`str-reverse`](#link7374722d72657665727365).	 [→index](#idx) [→topic](#array)

## `array-set` : procedure/3 {#link61727261792d736574}

Usage: `(array-set arr idx value)`

Set the value at index `idx` in `arr` to `value`. Arrays are 0-indexed. This mutates the array.

See also: [`array?`](#link61727261793f), [`array`](#link6172726179).	 [→index](#idx) [→topic](#array)

## `array-slice` : procedure/3 {#link61727261792d736c696365}

Usage: `(array-slice arr low high) => array`

Slice the array `arr` starting from `low` (inclusive) and ending at `high` (exclusive) and return the slice. This function is destrcutive and mutates the slice. Use array-copy if you need a copy.

See also: [`array-ref`](#link61727261792d726566), [`array-len`](#link61727261792d6c656e), [`array-append`](#link61727261792d617070656e64), [`build-array`](#link6275696c642d6172726179), [`array`](#link6172726179), [`array-copy`](#link61727261792d636f7079), [`array+`](#link61727261792b).	 [→index](#idx) [→topic](#array)

## `array-sort` : procedure/2 {#link61727261792d736f7274}

Usage: `(array-sort arr proc) => arr`

Destructively sorts array `arr` by using comparison proc `proc`, which takes two arguments and returns true if the first argument is smaller than the second argument, nil otherwise. The array is returned but it is not copied and modified in place by this procedure. The sorting algorithm is not guaranteed to be stable.

See also: [`sort`](#link736f7274).	 [→index](#idx) [→topic](#array)

## `array-walk` : procedure/2 {#link61727261792d77616c6b}

Usage: `(array-walk arr proc)`

Traverse the array `arr` from first to last element and apply `proc` to each element for side-effects. Function `proc` takes the index and the array element at that index as argument. If `proc` returns nil, then the traversal stops and the index is returned. If `proc` returns non-nil, traversal continues. If `proc` never returns nil, then the index returned is -1. This function does not mutate the array.

See also: [`array-map!`](#link61727261792d6d617021), [`array-pmap!`](#link61727261792d706d617021), [`array?`](#link61727261793f), [`map`](#link6d6170), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#array)

## `array?` : procedure/1 {#link61727261793f}

Usage: `(array? obj) => bool`

Return true of `obj` is an array, nil otherwise.

See also: [`seq?`](#link7365713f), [`array`](#link6172726179).	 [→index](#idx) [→topic](#array)

## `ascii85->blob` : procedure/1 {#link617363696938352d3e626c6f62}

Usage: `(ascii85->blob str) => blob`

Convert the ascii85 encoded string `str` to a binary blob. This will raise an error if `str` is not a valid ascii85 encoded string.

See also: [`blob->ascii85`](#link626c6f622d3e61736369693835), [`base64->blob`](#link6261736536342d3e626c6f62), [`str->blob`](#link7374722d3e626c6f62), [`hex->blob`](#link6865782d3e626c6f62).	 [→index](#idx) [→topic](#conversion)

## `assoc` : procedure/2 {#link6173736f63}

Usage: `(assoc key alist) => li`

Return the sublist of `alist` that starts with `key` if there is any, nil otherwise. Testing is done with equal?. An association list may be of the form ((key1 value1)(key2 value2)...) or ((key1 . value1) (key2 . value2) ...)

See also: [`assoc`](#link6173736f63), [`assoc1`](#link6173736f6331), [`alist?`](#link616c6973743f), [`eq?`](#link65713f), [`equal?`](#link657175616c3f).	 [→index](#idx) [→topic](#lisp)

## `assoc1` : procedure/2 {#link6173736f6331}

Usage: `(assoc1 sym li) => any`

Get the second element in the first sublist in `li` that starts with `sym`. This is equivalent to (cadr (assoc sym li)).

See also: [`assoc`](#link6173736f63), [`alist?`](#link616c6973743f).	 [→index](#idx) [→topic](#lisp)

## `assq` : procedure/2 {#link61737371}

Usage: `(assq key alist) => li`

Return the sublist of `alist` that starts with `key` if there is any, nil otherwise. Testing is done with eq?. An association list may be of the form ((key1 value1)(key2 value2)...) or ((key1 . value1) (key2 . value2) ...)

See also: [`assoc`](#link6173736f63), [`assoc1`](#link6173736f6331), [`eq?`](#link65713f), [`alist?`](#link616c6973743f), [`equal?`](#link657175616c3f).	 [→index](#idx) [→topic](#lisp)

## `atom?` : procedure/1 {#link61746f6d3f}

Usage: `(atom? x) => bool`

Return true if `x` is an atomic value, nil otherwise. Atomic values are numbers and symbols.

See also: [`sym?`](#link73796d3f).	 [→index](#idx) [→topic](#lisp)

## `base64->blob` : procedure/1 {#link6261736536342d3e626c6f62}

Usage: `(base64->blob str) => blob`

Convert the base64 encoded string `str` to a binary blob. This will raise an error if `str` is not a valid base64 encoded string.

See also: [`blob->base64`](#link626c6f622d3e626173653634), [`hex->blob`](#link6865782d3e626c6f62), [`ascii85->blob`](#link617363696938352d3e626c6f62), [`str->blob`](#link7374722d3e626c6f62).	 [→index](#idx) [→topic](#conversion)

## `beep` : procedure/1 {#link62656570}

Usage: `(beep sel)`

Play a built-in system sound. The argument `sel` may be one of '(error start ready click okay confirm info).

See also: [`set-volume`](#link7365742d766f6c756d65).	 [→index](#idx) [→topic](#sound)

## `bind` : procedure/2 {#link62696e64}

Usage: `(bind sym value)`

Bind `value` to the global symbol `sym`. In contrast to setq both values need quoting.

See also: [`setq`](#link73657471).	 [→index](#idx) [→topic](#system)

## `bitand` : procedure/2 {#link626974616e64}

Usage: `(bitand n m) => int`

Return the bitwise and of integers `n` and `m.`

See also: [`bitxor`](#link626974786f72), [`bitor`](#link6269746f72), [`bitclear`](#link626974636c656172), [`bitshl`](#link62697473686c), [`bitshr`](#link626974736872).	 [→index](#idx) [→topic](#binary)

## `bitclear` : procedure/2 {#link626974636c656172}

Usage: `(bitclear n m) => int`

Return the bitwise and-not of integers `n` and `m.`

See also: [`bitxor`](#link626974786f72), [`bitand`](#link626974616e64), [`bitor`](#link6269746f72), [`bitshl`](#link62697473686c), [`bitshr`](#link626974736872).	 [→index](#idx) [→topic](#binary)

## `bitor` : procedure/2 {#link6269746f72}

Usage: `(bitor n m) => int`

Return the bitwise or of integers `n` and `m.`

See also: [`bitxor`](#link626974786f72), [`bitand`](#link626974616e64), [`bitclear`](#link626974636c656172), [`bitshl`](#link62697473686c), [`bitshr`](#link626974736872).	 [→index](#idx) [→topic](#binary)

## `bitshl` : procedure/2 {#link62697473686c}

Usage: `(bitshl n m) => int`

Return the bitwise left shift of `n` by `m.`

See also: [`bitxor`](#link626974786f72), [`bitor`](#link6269746f72), [`bitand`](#link626974616e64), [`bitclear`](#link626974636c656172), [`bitshr`](#link626974736872).	 [→index](#idx) [→topic](#binary)

## `bitshr` : procedure/2 {#link626974736872}

Usage: `(bitshr n m) => int`

Return the bitwise right shift of `n` by `m.`

See also: [`bitxor`](#link626974786f72), [`bitor`](#link6269746f72), [`bitand`](#link626974616e64), [`bitclear`](#link626974636c656172), [`bitshl`](#link62697473686c).	 [→index](#idx) [→topic](#binary)

## `bitxor` : procedure/2 {#link626974786f72}

Usage: `(bitxor n m) => int`

Return the bitwise exclusive or value of integers `n` and `m.`

See also: [`bitand`](#link626974616e64), [`bitor`](#link6269746f72), [`bitclear`](#link626974636c656172), [`bitshl`](#link62697473686c), [`bitshr`](#link626974736872).	 [→index](#idx) [→topic](#binary)

## `blob->ascii85` : procedure/1 or more {#link626c6f622d3e61736369693835}

Usage: `(blob->ascii85 b [start] [end]) => str`

Convert the blob `b` to an ascii85 encoded string. If the optional `start` and `end` are provided, then only bytes from `start` (inclusive) to `end` (exclusive) are converted.

See also: [`blob->hex`](#link626c6f622d3e686578), [`blob->str`](#link626c6f622d3e737472), [`blob->base64`](#link626c6f622d3e626173653634), [`valid?`](#link76616c69643f), [`blob?`](#link626c6f623f).	 [→index](#idx) [→topic](#conversion)

## `blob->base64` : procedure/1 or more {#link626c6f622d3e626173653634}

Usage: `(blob->base64 b [start] [end]) => str`

Convert the blob `b` to a base64 encoded string. If the optional `start` and `end` are provided, then only bytes from `start` (inclusive) to `end` (exclusive) are converted.

See also: [`base64->blob`](#link6261736536342d3e626c6f62), [`valid?`](#link76616c69643f), [`blob?`](#link626c6f623f), [`blob->str`](#link626c6f622d3e737472), [`blob->hex`](#link626c6f622d3e686578), [`blob->ascii85`](#link626c6f622d3e61736369693835).	 [→index](#idx) [→topic](#conversion)

## `blob->hex` : procedure/1 or more {#link626c6f622d3e686578}

Usage: `(blob->hex b [start] [end]) => str`

Convert the blob `b` to a hexadecimal string of byte values. If the optional `start` and `end` are provided, then only bytes from `start` (inclusive) to `end` (exclusive) are converted.

See also: [`hex->blob`](#link6865782d3e626c6f62), [`str->blob`](#link7374722d3e626c6f62), [`valid?`](#link76616c69643f), [`blob?`](#link626c6f623f), [`blob->base64`](#link626c6f622d3e626173653634), [`blob->ascii85`](#link626c6f622d3e61736369693835).	 [→index](#idx) [→topic](#conversion)

## `blob->str` : procedure/1 or more {#link626c6f622d3e737472}

Usage: `(blob->str b [start] [end]) => str`

Convert blob `b` into a string. Notice that the string may contain binary data that is not suitable for displaying and does not represent valid UTF-8 glyphs. If the optional `start` and `end` are provided, then only bytes from `start` (inclusive) to `end` (exclusive) are converted.

See also: [`str->blob`](#link7374722d3e626c6f62), [`valid?`](#link76616c69643f), [`blob?`](#link626c6f623f).	 [→index](#idx) [→topic](#conversion)

## `blob-chksum` : procedure/1 or more {#link626c6f622d63686b73756d}

Usage: `(blob-chksum b [start] [end]) => blob`

Return the checksum of the contents of blob `b` as new blob. The checksum is cryptographically secure. If the optional `start` and `end` are provided, then only the bytes from `start` (inclusive) to `end` (exclusive) are checksummed.

See also: [`fchksum`](#link6663686b73756d), [`blob-free`](#link626c6f622d66726565).	 [→index](#idx) [→topic](#binary)

## `blob-equal?` : procedure/2 {#link626c6f622d657175616c3f}

Usage: `(blob-equal? b1 b2) => bool`

Return true if `b1` and `b2` are equal, nil otherwise. Two blobs are equal if they are either both invalid, both contain no valid data, or their contents contain exactly the same binary data.

See also: [`str->blob`](#link7374722d3e626c6f62), [`blob->str`](#link626c6f622d3e737472), [`blob-free`](#link626c6f622d66726565).	 [→index](#idx) [→topic](#binary)

## `blob-free` : procedure/1 {#link626c6f622d66726565}

Usage: `(blob-free b)`

Frees the binary data stored in blob `b` and makes the blob invalid.

See also: [`make-blob`](#link6d616b652d626c6f62), [`valid?`](#link76616c69643f), [`str->blob`](#link7374722d3e626c6f62), [`blob->str`](#link626c6f622d3e737472), [`blob-equal?`](#link626c6f622d657175616c3f).	 [→index](#idx) [→topic](#binary)

## `blob?` : procedure/1 {#link626c6f623f}

Usage: `(blob? obj) => bool`

Return true if `obj` is a binary blob, nil otherwise.

See also: [`blob->ascii85`](#link626c6f622d3e61736369693835), [`blob->base64`](#link626c6f622d3e626173653634), [`blob->hex`](#link626c6f622d3e686578), [`blob->str`](#link626c6f622d3e737472), [`blob-free`](#link626c6f622d66726565), [`blob-chksum`](#link626c6f622d63686b73756d), [`blob-equal?`](#link626c6f622d657175616c3f), [`valid?`](#link76616c69643f).	 [→index](#idx) [→topic](#binary)

## `bool?` : procedure/1 {#link626f6f6c3f}

Usage: `(bool? datum) => bool`

Return true if `datum` is either true or nil. Note: This predicate only exists for type-completeness and you should never use it as part of testing whether something is true or false - per convention, a value is true if it is non-nil and not when it is true, which is the special boolean value this predicate tests in addition to nil.

See also: [`null?`](#link6e756c6c3f), [`not`](#link6e6f74).	 [→index](#idx) [→topic](#lisp)

## `bound?` : macro/1 {#link626f756e643f}

Usage: `(bound? sym) => bool`

Return true if a value is bound to the symbol `sym`, nil otherwise.

See also: [`bind`](#link62696e64), [`setq`](#link73657471).	 [→index](#idx) [→topic](#system)

## `boxed?` : procedure/1 {#link626f7865643f}

Usage: `(boxed? x) => bool`

Return true if `x` is a boxed value, nil otherwise. Boxed values are special objects that are special in the system and sometimes cannot be garbage collected.

See also: [`type-of`](#link747970652d6f66), [`num?`](#link6e756d3f), [`str?`](#link7374723f), [`sym?`](#link73796d3f), [`list?`](#link6c6973743f), [`array?`](#link61727261793f), [`macro?`](#link6d6163726f3f), [`closure?`](#link636c6f737572653f), [`intrinsic?`](#link696e7472696e7369633f), [`eof?`](#link656f663f).	 [→index](#idx) [→topic](#system)

## `build-array` : procedure/2 {#link6275696c642d6172726179}

Usage: `(build-array n init) => array`

Create an array containing `n` elements with initial value `init.`

See also: [`array`](#link6172726179), [`array?`](#link61727261793f), [`array-slice`](#link61727261792d736c696365), [`array-append`](#link61727261792d617070656e64), [`array-copy`](#link61727261792d636f7079), [`array+`](#link61727261792b).	 [→index](#idx) [→topic](#array)

## `build-list` : procedure/2 {#link6275696c642d6c697374}

Usage: `(build-list n proc) => list`

Build a list with `n` elements by applying `proc` to the counter `n` each time.

See also: [`list`](#link6c697374), [`list?`](#link6c6973743f), [`map`](#link6d6170), [`foreach`](#link666f7265616368).	 [→index](#idx) [→topic](#lisp)

## `caaar` : procedure/1 {#link6361616172}

Usage: `(caaar x) => any`

Equivalent to (car (car (car `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `caadr` : procedure/1 {#link6361616472}

Usage: `(caadr x) => any`

Equivalent to (car (car (cdr `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `caar` : procedure/1 {#link63616172}

Usage: `(caar x) => any`

Equivalent to (car (car `x`)).

See also: [`car`](#link636172), [`cdr`](#link636472), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `cadar` : procedure/1 {#link6361646172}

Usage: `(cadar x) => any`

Equivalent to (car (cdr (car `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `caddr` : procedure/1 {#link6361646472}

Usage: `(caddr x) => any`

Equivalent to (car (cdr (cdr `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `cadr` : procedure/1 {#link63616472}

Usage: `(cadr x) => any`

Equivalent to (car (cdr `x`)).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `call-method` : procedure/3 {#link63616c6c2d6d6574686f64}

Usage: `(call-method obj mname args) => any`

Execute method `mname` of object `obj` with additional arguments in list `args`. The first argument in the method call is always `obj` itself.

See also: [`defmethod`](#link6465666d6574686f64), [`defclass`](#link646566636c617373), [`new`](#link6e6577), [`isa?`](#link6973613f), [`class-of`](#link636c6173732d6f66).	 [→index](#idx) [→topic](#oop)

## `call-super` : procedure/3 {#link63616c6c2d7375706572}

Usage: `(call-super obj mname args) => any`

Execute method `mname` of the first superclass of `obj` that has a method with that name.

See also: [`call-method`](#link63616c6c2d6d6574686f64), [`supers`](#link737570657273).	 [→index](#idx) [→topic](#oop)

## `can-externalize?` : procedure/1 {#link63616e2d65787465726e616c697a653f}

Usage: `(can-externalize? datum) => bool`

Recursively determines if `datum` can be externalized and returns true in this case, nil otherwise.

See also: [`externalize`](#link65787465726e616c697a65), [`externalize0`](#link65787465726e616c697a6530).	 [→index](#idx) [→topic](#system)

## `car` : procedure/1 {#link636172}

Usage: `(car li) => any`

Get the first element of a list or pair `li`, an error if there is not first element.

See also: [`list`](#link6c697374), [`list?`](#link6c6973743f), [`pair?`](#link706169723f).	 [→index](#idx) [→topic](#lisp)

## `case` : macro/2 or more {#link63617365}

Usage: `(case expr (clause1 ... clausen)) => any`

Standard case macro, where you should use t for the remaining alternative. Example: (case (get dict 'key) ((a b) (out "a or b"))(t (out "something else!"))).

See also: [`cond`](#link636f6e64).	 [→index](#idx) [→topic](#lisp)

## `ccmp` : macro/2 {#link63636d70}

Usage: `(ccmp sym value) => int`

Compare the integer value of `sym` with the integer `value`, return 0 if `sym` = `value`, -1 if `sym` < `value`, and 1 if `sym` > `value`. This operation is synchronized between tasks and futures.

See also: [`cinc!`](#link63696e6321), [`cdec!`](#link6364656321), [`cwait`](#link6377616974), [`cst!`](#link63737421).	 [→index](#idx) [→topic](#concurrency)

## `cdaar` : procedure/1 {#link6364616172}

Usage: `(cdaar x) => any`

Equivalent to (cdr (car (car `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `cdadr` : procedure/1 {#link6364616472}

Usage: `(cdadr x) => any`

Equivalent to (cdr (car (cdr `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `cdar` : procedure/1 {#link63646172}

Usage: `(cdar x) => any`

Equivalent to (cdr (car `x`)).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `cddar` : procedure/1 {#link6364646172}

Usage: `(cddar x) => any`

Equivalent to (cdr (cdr (car `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `cdddr` : procedure/1 {#link6364646472}

Usage: `(cdddr x) => any`

Equivalent to (cdr (cdr (cdr `x`))).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`cddr`](#link63646472), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `cddr` : procedure/1 {#link63646472}

Usage: `(cddr x) => any`

Equivalent to (cdr (cdr `x`)).

See also: [`car`](#link636172), [`cdr`](#link636472), [`caar`](#link63616172), [`cadr`](#link63616472), [`cdar`](#link63646172), [`caaar`](#link6361616172), [`caadr`](#link6361616472), [`cadar`](#link6361646172), [`caddr`](#link6361646472), [`cdaar`](#link6364616172), [`cdadr`](#link6364616472), [`cddar`](#link6364646172), [`cdddr`](#link6364646472), [`nth`](#link6e7468), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264).	 [→index](#idx) [→topic](#lisp)

## `cdec!` : macro/1 {#link6364656321}

Usage: `(cdec! sym) => int`

Decrease the integer value stored in top-level symbol `sym` by 1 and return the new value. This operation is synchronized between tasks and futures.

See also: [`cinc!`](#link63696e6321), [`cwait`](#link6377616974), [`ccmp`](#link63636d70), [`cst!`](#link63737421).	 [→index](#idx) [→topic](#concurrency)

## `cdr` : procedure/1 {#link636472}

Usage: `(cdr li) => any`

Get the rest of a list `li`. If the list is proper, the cdr is a list. If it is a pair, then it may be an element. If the list is empty, nil is returned.

See also: [`car`](#link636172), [`list`](#link6c697374), [`list?`](#link6c6973743f), [`pair?`](#link706169723f).	 [→index](#idx) [→topic](#lisp)

## `center-window-on-screen` : procedure/1 {#link63656e7465722d77696e646f772d6f6e2d73637265656e}

Usage: `(center-window-on-screen window)`

As the name implies, this function centers the window on the screen.

See also: [`set-window-full-screen`](#link7365742d77696e646f772d66756c6c2d73637265656e), [`window-full-screen?`](#link77696e646f772d66756c6c2d73637265656e3f).	 [→index](#idx) [→topic](#gui)

## `change-action-prefix` : procedure/2 {#link6368616e67652d616374696f6e2d707265666978}

Usage: `(change-action-prefix id new-prefix) => bool`

Change the prefix of a registered action with given `id`, or change the prefix of action given by `id`, to `new-prefix`. If the operation succeeds, it returns true, otherwise it returns nil.

See also: [`change-all-action-prefixes`](#link6368616e67652d616c6c2d616374696f6e2d7072656669786573), [`rename-action`](#link72656e616d652d616374696f6e), [`get-action`](#link6765742d616374696f6e), [`action?`](#link616374696f6e3f), [`action`](#link616374696f6e).	 [→index](#idx) [→topic](#action)

## `change-all-action-prefixes` : procedure/2 {#link6368616e67652d616c6c2d616374696f6e2d7072656669786573}

Usage: `(change-all-action-prefixes old-prefix new-prefix)`

Change the prefixes of all registered actions with `old-prefix` to `new-prefix.`

See also: [`change-action-prefix`](#link6368616e67652d616374696f6e2d707265666978), [`rename-action`](#link72656e616d652d616374696f6e), [`get-action`](#link6765742d616374696f6e), [`register-action`](#link72656769737465722d616374696f6e), [`action?`](#link616374696f6e3f), [`action`](#link616374696f6e).	 [→index](#idx) [→topic](#action)

## `char->str` : procedure/1 {#link636861722d3e737472}

Usage: `(char->str n) => str`

Return a string containing the unicode char based on integer `n.`

See also: [`str->char`](#link7374722d3e63686172).	 [→index](#idx) [→topic](#conversion)

## `chars` : procedure/1 {#link6368617273}

Usage: `(chars str) => dict`

Return a charset based on `str`, i.e., dict with the chars of `str` as keys and true as value.

See also: [`dict`](#link64696374), [`get`](#link676574), [`set`](#link736574), [`contains`](#link636f6e7461696e73).	 [→index](#idx) [→topic](#data)

## `chars->str` : procedure/1 {#link63686172732d3e737472}

Usage: `(chars->str a) => str`

Convert an array of UTF-8 rune integers `a` into a UTF-8 encoded string.

See also: [`str->runes`](#link7374722d3e72756e6573), [`str->char`](#link7374722d3e63686172), [`char->str`](#link636861722d3e737472).	 [→index](#idx) [→topic](#conversion)

## `cinc!` : macro/1 {#link63696e6321}

Usage: `(cinc! sym) => int`

Increase the integer value stored in top-level symbol `sym` by 1 and return the new value. This operation is synchronized between tasks and futures.

See also: [`cdec!`](#link6364656321), [`cwait`](#link6377616974), [`ccmp`](#link63636d70), [`cst!`](#link63737421).	 [→index](#idx) [→topic](#concurrency)

## `class-name` : procedure/1 {#link636c6173732d6e616d65}

Usage: `(class-name c) => sym`

Return the name of a class `c`. An error occurs if `c` is not a valid class.

See also: [`class?`](#link636c6173733f), [`isa?`](#link6973613f).	 [→index](#idx) [→topic](#oop)

## `class-of` : procedure/1 {#link636c6173732d6f66}

Usage: `(class-of obj) => class or nil`

Return the class of object `obj`, nil if `obj` is not a valid object array.

See also: [`new`](#link6e6577), [`isa?`](#link6973613f).	 [→index](#idx) [→topic](#oop)

## `class?` : procedure/1 {#link636c6173733f}

Usage: `(class? c) => bool`

Return true if `c` is a class array (not a name for a class!), nil otherwise.

See also: [`object?`](#link6f626a6563743f), [`isa?`](#link6973613f).	 [→index](#idx) [→topic](#oop)

## `close` : procedure/1 {#link636c6f7365}

Usage: `(close p)`

Close the port `p`. Calling close twice on the same port should be avoided.

See also: [`open`](#link6f70656e), [`stropen`](#link7374726f70656e).	 [→index](#idx) [→topic](#fileio)

## `close-gui` : procedure/0 {#link636c6f73652d677569}

Usage: `(close-gui)`

Close the GUI, freeing all resources associated with it. After this function has been called, no further GUI functions can be used.

See also: [`forget-gui-object`](#link666f726765742d6775692d6f626a656374), [`close-window`](#link636c6f73652d77696e646f77).	 [→index](#idx) [→topic](#gui)

## `close-window` : procedure/1 {#link636c6f73652d77696e646f77}

Usage: `(close-window window)`

Closes `window` and delete it from internal storage. This frees window resources. It cannot be re-used after this operation. Use window-hide if you want to close/hide a window only temporarily. Notice that unlike in Fyne, there is no mechanism to close an application automatically after its main window has been closed.

See also: [`hide-window`](#link686964652d77696e646f77).	 [→index](#idx) [→topic](#gui)

## `closure?` : procedure/1 {#link636c6f737572653f}

Usage: `(closure? x) => bool`

Return true if `x` is a closure, nil otherwise. Use `function?` for texting whether `x` can be executed.

See also: [`functional?`](#link66756e6374696f6e616c3f), [`macro?`](#link6d6163726f3f), [`intrinsic?`](#link696e7472696e7369633f), [`functional-arity`](#link66756e6374696f6e616c2d6172697479), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx) [→topic](#system)

## `collect-garbage` : procedure/0 or more {#link636f6c6c6563742d67617262616765}

Usage: `(collect-garbage [sort])`

Force a garbage-collection of the system's memory. If `sort` is 'normal, then only a normal incremental garbage colllection is performed. If `sort` is 'total, then the garbage collection is more thorough and the system attempts to return unused memory to the host OS. Default is 'normal.

See also: [`memstats`](#link6d656d7374617473).	 [→index](#idx)

**Warning: There should rarely be a use for this. Try to use less memory-consuming data structures instead.** [→topic](#system)

## `color` : procedure/1 {#link636f6c6f72}

Usage: `(color sel) => (r g b a)`

Return the color based on `sel`, which may be 'text for the text color, 'back for the background color, 'textarea for the color of the text area, 'gfx for the current graphics foreground color, and 'frame for the frame color. In standard Z3S5 Lisp only 'text and 'back are available as selectors and implementations are free to ignore these.

See also: [`set-color`](#link7365742d636f6c6f72), [`reset-color`](#link72657365742d636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`with-colors`](#link776974682d636f6c6f7273).	 [→index](#idx) [→topic](#ui)

## `color->color64` : procedure/1 {#link636f6c6f722d3e636f6c6f723634}

Usage: `(color->color64 li) => li`

Convert a 32-bit NRGBA color list with values from 0 to 255 (inclusive) as it is used by Z3S5 Lisp's *color* dict to a 64-bit NRGBA64 color list with values from 0 to 65635 (inclusive) as they are used by the GUI.

See also: [`theme-color`](#link7468656d652d636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a).	 [→index](#idx) [→topic](#gui)

## `color64->color` : procedure/1 {#link636f6c6f7236342d3e636f6c6f72}

Usage: `(color64->color li) => li`

Convert a 64-bit NRGBA64 color list with values from 0 to 65365 (inclusive) to a 32-bit color list with values from 0 to 255 (inclusive) as they are used by Z3S5 Lisp's *colors* dict.

See also: [`theme-color`](#link7468656d652d636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a).	 [→index](#idx) [→topic](#gui)

## `cons` : procedure/2 {#link636f6e73}

Usage: `(cons a b) => pair`

Cons two values into a pair. If `b` is a list, the result is a list. Otherwise the result is a pair.

See also: [`cdr`](#link636472), [`car`](#link636172), [`list?`](#link6c6973743f), [`pair?`](#link706169723f).	 [→index](#idx) [→topic](#lisp)

## `cons?` : procedure/1 {#link636f6e733f}

Usage: `(cons? x) => bool`

return true if `x` is not an atom, nil otherwise.

See also: [`atom?`](#link61746f6d3f).	 [→index](#idx) [→topic](#lisp)

## `copy-record` : procedure/1 {#link636f70792d7265636f7264}

Usage: `(copy-record r) => record`

Creates a non-recursive, shallow copy of record `r.`

See also: [`record?`](#link7265636f72643f).	 [→index](#idx) [→topic](#oop)

## `count-partitions` : procedure/2 {#link636f756e742d706172746974696f6e73}

Usage: `(count-partitions m k) => int`

Return the number of partitions for divding `m` items into parts of size `k` or less, where the size of the last partition may be less than `k` but the remaining ones have size `k.`

See also: [`nth-partition`](#link6e74682d706172746974696f6e), [`get-partitions`](#link6765742d706172746974696f6e73).	 [→index](#idx) [→topic](#lisp)

## `count-text-grid-row-columns` : procedure/2 {#link636f756e742d746578742d677269642d726f772d636f6c756d6e73}

Usage: `(count-text-grid-row-columns grid row) => int`

Return the number of columns in `row` of `grid`, 0 if there are none.

See also: [`count-text-grid-rows`](#link636f756e742d746578742d677269642d726f7773), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77).	 [→index](#idx) [→topic](#gui)

## `count-text-grid-rows` : procedure/1 {#link636f756e742d746578742d677269642d726f7773}

Usage: `(count-text-grid-rows grid) => int`

Return the number of the last row in grid, 0 if there are none.

See also: [`count-text-grid-row-columns`](#link636f756e742d746578742d677269642d726f772d636f6c756d6e73), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77).	 [→index](#idx) [→topic](#gui)

## `cpunum` : procedure/0 {#link6370756e756d}

Usage: `(cpunum)`

Return the number of cpu cores of this machine.

See also: [`sys`](#link737973).	 [→index](#idx)

**Warning: This function also counts virtual cores on the emulator. The original Z3S5 machine did not have virtual cpu cores.** [→topic](#concurrency)

## `create-lorem-ipsum` : procedure/3 {#link6372656174652d6c6f72656d2d697073756d}

Usage: `(create-lorem-ipsum selector min max) => str`

Create random Lorem Ipsum fill text based on `selector`. If `selector` is 'word, then a word with at least `min` letters and at most `max` letters is created. If `selector` is 'sentence, then a sentence with at least `min` words and at most `max` words is created. If `selector` is 'paragraph, then a paragraph with at least `min` sentences and at most `max` sentences is created.

See also: [`new-zedit`](#link6e65772d7a65646974), [`new-text-grid`](#link6e65772d746578742d67726964), [`new-entry`](#link6e65772d656e747279).	 [→index](#idx) [→topic](#gui)

## `cst!` : procedure/2 {#link63737421}

Usage: `(cst! sym value)`

Set the value of `sym` to integer `value`. This operation is synchronized between tasks and futures.

See also: [`cinc!`](#link63696e6321), [`cdec!`](#link6364656321), [`ccmp`](#link63636d70), [`cwait`](#link6377616974).	 [→index](#idx) [→topic](#concurrency)

## `current-error-handler` : procedure/0 {#link63757272656e742d6572726f722d68616e646c6572}

Usage: `(current-error-handler) => proc`

Return the current error handler, a default if there is none.

See also: [`default-error-handler`](#link64656661756c742d6572726f722d68616e646c6572), [`push-error-handler`](#link707573682d6572726f722d68616e646c6572), [`pop-error-handler`](#link706f702d6572726f722d68616e646c6572), [`*current-error-handler*`](#link2a63757272656e742d6572726f722d68616e646c65722a), [`*current-error-continuation*`](#link2a63757272656e742d6572726f722d636f6e74696e756174696f6e2a).	 [→index](#idx) [→topic](#system)

## `current-zimage` : procedure/0 {#link63757272656e742d7a696d616765}

Usage: `(current-zimage [nonce]) => dict`

Obtain a dict of all toplevel bindings. If the `nonce` is provided, procedures are externalized as (nonce proc) to distinguish them from data. This function may use a lot of memory. Consider saving or loading zimages directly from disk instead. Notice that the dict is not the same format as the one used by load-zimage and save-zimage.

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`save-zimage`](#link736176652d7a696d616765), [`externalize`](#link65787465726e616c697a65).	 [→index](#idx) [→topic](#zimage)

## `cwait` : procedure/3 {#link6377616974}

Usage: `(cwait sym value timeout)`

Wait until integer counter `sym` has `value` or `timeout` milliseconds have passed. If `imeout` is 0, then this routine might wait indefinitely. This operation is synchronized between tasks and futures.

See also: [`cinc!`](#link63696e6321), [`cdec!`](#link6364656321), [`ccmp`](#link63636d70), [`cst!`](#link63737421).	 [→index](#idx) [→topic](#concurrency)

## `darken` : procedure/1 {#link6461726b656e}

Usage: `(darken color [amount]) => (r g b a)`

Return a darker version of `color`. The optional positive `amount` specifies the amount of darkening (0-255).

See also: [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a), [`lighten`](#link6c69676874656e).	 [→index](#idx) [→topic](#ui)

## `date->epoch-ns` : procedure/7 {#link646174652d3e65706f63682d6e73}

Usage: `(date->epoch-ns Y M D h m s ns) => int`

Return the Unix epoch nanoseconds based on the given year `Y`, month `M`, day `D`, hour `h`, minute `m`, seconds `s`, and nanosecond fraction of a second `ns`, as it is e.g. returned in a (now) datelist.

See also: [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`datestr`](#link64617465737472), [`datestr*`](#link646174657374722a), [`day-of-week`](#link6461792d6f662d7765656b), [`week-of-date`](#link7765656b2d6f662d64617465), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `datelist->epoch-ns` : procedure/1 {#link646174656c6973742d3e65706f63682d6e73}

Usage: `(datelist->epoch-ns dateli) => int`

Convert a datelist to Unix epoch nanoseconds. This function uses the Unix nanoseconds from the 5th value of the second list in the datelist, as it is provided by functions like (now). However, if the Unix nanoseconds value is not specified in the list, it uses `date->epoch-ns` to convert to Unix epoch nanoseconds. Datelists can be incomplete. If the month is not specified, January is assumed. If the day is not specified, the 1st is assumed. If the hour is not specified, 12 is assumed, and corresponding defaults for minutes, seconds, and nanoseconds are 0.

See also: [`date->epoch-ns`](#link646174652d3e65706f63682d6e73), [`datestr`](#link64617465737472), [`datestr*`](#link646174657374722a), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `datestr` : procedure/1 {#link64617465737472}

Usage: `(datestr datelist) => str`

Return datelist, as it is e.g. returned by (now), as a string in format YYYY-MM-DD HH:mm.

See also: [`now`](#link6e6f77), [`datestr*`](#link646174657374722a), [`datestr->datelist`](#link646174657374722d3e646174656c697374).	 [→index](#idx) [→topic](#time)

## `datestr*` : procedure/1 {#link646174657374722a}

Usage: `(datestr* datelist) => str`

Return the datelist, as it is e.g. returned by (now), as a string in format YYYY-MM-DD HH:mm:ss.nanoseconds.

See also: [`now`](#link6e6f77), [`datestr`](#link64617465737472), [`datestr->datelist`](#link646174657374722d3e646174656c697374).	 [→index](#idx) [→topic](#time)

## `datestr->datelist` : procedure/1 {#link646174657374722d3e646174656c697374}

Usage: `(datestr->datelist s) => li`

Convert a date string in the format of datestr and datestr* into a date list as it is e.g. returned by (now).

See also: [`datestr*`](#link646174657374722a), [`datestr`](#link64617465737472), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `day+` : procedure/2 {#link6461792b}

Usage: `(day+ dateli n) => dateli`

Adds `n` days to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`minute+`](#link6d696e7574652b), [`hour+`](#link686f75722b), [`week+`](#link7765656b2b), [`month+`](#link6d6f6e74682b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `day-of-week` : procedure/3 {#link6461792d6f662d7765656b}

Usage: `(day-of-week Y M D) => int`

Return the day of week based on the date with year `Y`, month `M`, and day `D`. The first day number 0 is Sunday, the last day is Saturday with number 6.

See also: [`week-of-date`](#link7765656b2d6f662d64617465), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`date->epoch-ns`](#link646174652d3e65706f63682d6e73), [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374), [`datestr`](#link64617465737472), [`datestr*`](#link646174657374722a), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `db.blob` : procedure/2 {#link64622e626c6f62}

Usage: `(db.blob db-result n) => fl`

Get the content of column `n` in `db-result` as blob. A blob is a boxed memory area holding binary data.

See also: [`db.str`](#link64622e737472).	 [→index](#idx) [→topic](#db)

## `db.close` : procedure/1 {#link64622e636c6f7365}

Usage: `(db.close db)`

Close the database `db.`

See also: [`db.open`](#link64622e6f70656e), [`db.open*`](#link64622e6f70656e2a), [`db.exec`](#link64622e65786563), [`db.query`](#link64622e7175657279).	 [→index](#idx) [→topic](#db)

## `db.close-result` : procedure/1 {#link64622e636c6f73652d726573756c74}

Usage: `(db.close-result db-result)`

Close the `db-result`. It is invalid afterwards. This should be done to avoid memory leaks after the result has been used.

See also: [`db.reset`](#link64622e7265736574), [`db.step`](#link64622e73746570), [`db.close`](#link64622e636c6f7365).	 [→index](#idx) [→topic](#db)

## `db.exec` : procedure/2 or more {#link64622e65786563}

Usage: `(db.exec db stmt [args] ...)`

Execute the SQL statement `stmt` in database `db`, binding any optional `args` to the open variable slots in it. This function does not return anything, use db.query to execute a query that returns rows as result.

See also: [`db.query`](#link64622e7175657279), [`db.open`](#link64622e6f70656e), [`db.close`](#link64622e636c6f7365), [`db.open*`](#link64622e6f70656e2a).	 [→index](#idx) [→topic](#db)

## `db.float` : procedure/2 {#link64622e666c6f6174}

Usage: `(db.float db-result n) => fl`

Get the content of column `n` in `db-result` as float.

See also: [`db.int`](#link64622e696e74), [`db.str`](#link64622e737472).	 [→index](#idx) [→topic](#db)

## `db.int` : procedure/2 {#link64622e696e74}

Usage: `(db.int db-result n) => int`

Get the content of column `n` in `db-result` as integer.

See also: [`db.float`](#link64622e666c6f6174), [`db.str`](#link64622e737472), [`db.blob`](#link64622e626c6f62).	 [→index](#idx) [→topic](#db)

## `db.open` : procedure/1 {#link64622e6f70656e}

Usage: `(db.open fi) => db`

Opens an sqlite3 DB or creates a new, empty database at file path `fi.`

See also: [`db.close`](#link64622e636c6f7365), [`db.exec`](#link64622e65786563), [`db.query`](#link64622e7175657279).	 [→index](#idx) [→topic](#db)

## `db.open*` : procedure/1 {#link64622e6f70656e2a}

Usage: `(db.open* sel) => db`

Open a temporary database if `sel` is 'temp or an in-memory database if `sel` is 'mem.

See also: [`db.open`](#link64622e6f70656e), [`db.close`](#link64622e636c6f7365), [`db.exec`](#link64622e65786563), [`db.query`](#link64622e7175657279).	 [→index](#idx) [→topic](#db)

## `db.query` : procedure/2 or more {#link64622e7175657279}

Usage: `(db.query db stmt [args] ...) => db-result`

Query `db` with SQL statement `stmt`, binding any optional `args` to the open variable slots in it. This function returns a `db-result` that can be used to loop through rows with db.step and obtain columns in them using the various accessor methods.

See also: [`db.exec`](#link64622e65786563), [`db.step`](#link64622e73746570), [`db.int`](#link64622e696e74), [`db.cname`](#link64622e636e616d65), [`db.float`](#link64622e666c6f6174), [`db.str`](#link64622e737472), [`db.expr`](#link64622e65787072), [`db.blob`](#link64622e626c6f62).	 [→index](#idx) [→topic](#db)

## `db.result-column-count` : procedure/1 {#link64622e726573756c742d636f6c756d6e2d636f756e74}

Usage: `(db.result-column-count db-result) => int`

Get the number of columns in the rows of `db-result.`

See also: [`db.result-columns`](#link64622e726573756c742d636f6c756d6e73).	 [→index](#idx) [→topic](#db)

## `db.result-columns` : procedure/1 {#link64622e726573756c742d636f6c756d6e73}

Usage: `(db.result-columns db-result) => li`

Get a list of column specifications for `db-result`, each consisting of a list with the column name and the column type as string, as these were provided to the query. Since queries support automatic type conversions, this need not reflect the column types in the database schema.

See also: [`db.result-column-count`](#link64622e726573756c742d636f6c756d6e2d636f756e74).	 [→index](#idx) [→topic](#db)

## `db.row` : procedure/1 {#link64622e726f77}

Usage: `(db.row db-result) => li`

Return all columns of the current row in `db-result` as list. They have the respective base types INT, FLOAT, BLOB, and TEXT.

See also: [`db.rows`](#link64622e726f7773).	 [→index](#idx) [→topic](#db)

## `db.step` : procedure/1 {#link64622e73746570}

Usage: `(db.step db-result) => bool`

Obtain the next result row in `db-result` and return true, or return nil of there is no more row in the result.

See also: [`db.query`](#link64622e7175657279), [`db.row`](#link64622e726f77), [`db.rows`](#link64622e726f7773).	 [→index](#idx) [→topic](#db)

## `db.str` : procedure/2 {#link64622e737472}

Usage: `(db.str db-result n) => str`

Get the content of column `n` in `db-result` as string.

See also: [`db.blob`](#link64622e626c6f62), [`db.int`](#link64622e696e74), [`db.float`](#link64622e666c6f6174).	 [→index](#idx) [→topic](#db)

## `deactivate-menu-child` : procedure/1 {#link646561637469766174652d6d656e752d6368696c64}

Usage: `(deactivate-menu-child menu)`

Deactivate the currently active menu item and close its submenu if there is one.

See also: [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75), [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874), [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573), [`new-menu`](#link6e65772d6d656e75).	 [→index](#idx) [→topic](#gui)

## `deactivate-menu-last-submenu` : procedure/1 {#link646561637469766174652d6d656e752d6c6173742d7375626d656e75}

Usage: `(deactivate-menu-last-submenu menu)`

Traverse the menu and deactivate the last open submenu found.

See also: [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75), [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874), [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573), [`new-menu`](#link6e65772d6d656e75).	 [→index](#idx) [→topic](#gui)

## `declare-volatile` : procedure/1 {#link6465636c6172652d766f6c6174696c65}

Usage: `(declare-volatile sym)`

Declares `sym`, which has to be quoted, as a volatile toplevel symbol. Volatile toplevel symbols are neither saved to nor loaded from zimages.

See also: [`save-zimage`](#link736176652d7a696d616765), [`load-zimage`](#link6c6f61642d7a696d616765), [`declare-unprotected`](#link6465636c6172652d756e70726f746563746564).	 [→index](#idx) [→topic](#zimage)

## `def-custom-hook` : procedure/2 {#link6465662d637573746f6d2d686f6f6b}

Usage: `(def-custom-hook sym proc)`

Define a custom hook point, to be called manually from Lisp. These have IDs starting from 65636.

See also: [`add-hook`](#link6164642d686f6f6b).	 [→index](#idx) [→topic](#system)

## `default-error-handler` : procedure/0 {#link64656661756c742d6572726f722d68616e646c6572}

Usage: `(default-error-handler) => proc`

Return the default error handler, irrespectively of the current-error-handler.

See also: [`current-error-handler`](#link63757272656e742d6572726f722d68616e646c6572), [`push-error-handler`](#link707573682d6572726f722d68616e646c6572), [`pop-error-handler`](#link706f702d6572726f722d68616e646c6572), [`*current-error-handler*`](#link2a63757272656e742d6572726f722d68616e646c65722a), [`*current-error-continuation*`](#link2a63757272656e742d6572726f722d636f6e74696e756174696f6e2a).	 [→index](#idx) [→topic](#system)

## `defclass` : macro/2 or more {#link646566636c617373}

Usage: `(defclass name supers [props] ...)`

Defines symbol `name` as class with superclasses `supers` and property clauses `props` listed as remaining arguments. A `props` clause is either a symbol for a property or a list of the form (sym default) for the property `sym` with `default` value. The class is bound to `name` and a class predicate `name?` is created. Argument `supers` may be a class name or a list of class names.

See also: [`defmethod`](#link6465666d6574686f64), [`new`](#link6e6577).	 [→index](#idx) [→topic](#oop)

## `defmacro` : macro/2 or more {#link6465666d6163726f}

Usage: `(defmacro name args body ...)`

Define a macro `name` with argument list `args` and `body`. Macros are expanded at compile-time.

See also: [`macro`](#link6d6163726f).	 [→index](#idx) [→topic](#lisp)

## `defmethod` : macro/2 or more {#link6465666d6574686f64}

Usage: `(defmethod class-name args [body] ...)`

Define a method `class-name` for class `class` and method name `name` with a syntax parallel to defun, where `args` are the arguments of the methods and `body` is the rest of the method. The given `class-name` must decompose into a valid class name `class` of a previously created class and method name `name` and is bound to the symbol `class-name`. The remaining arguments are like for defun. So for example (defmethod employee-name (this) (prop this 'last-name)) defines a method `name` for an existing class `employee` which retrieves the property `last-name`. Note that `defmethod` is dynamic: If you define a class B with class A as superclass, then B only inherits methods from A that have already been defined for A at the time of defining B!

See also: [`defclass`](#link646566636c617373), [`new`](#link6e6577), [`call-method`](#link63616c6c2d6d6574686f64).	 [→index](#idx) [→topic](#oop)

## `defstruct` : macro/1 or more {#link646566737472756374}

Usage: `(defstruct name props ...) => struct`

Binds symbol `name` to a struct with name `name` and with properties `props`. Each clause of `props` must be either a symbol for the property name or a list of the form (prop default-value) where `prop` is the symbol for the property name and `default-value` is the value it has by default. For each property `p`, accessors `name-p` and setters `name-p!` are created, as well as a function `name-p*` that takes a record `r`, a value `v`, and a procedure `proc` that takes no arguments. When `name-p*` is called on record `r`, it temporarily sets property `p` of `r` to the provided value `v` and calls the procedure `proc`. Afterwards, the original value of `p` is restored. Since this function mutates the record during the execution of `proc` and does not protect this operation against race conditions, it is not thread-safe. (But you can include a mutex as property and make it thread-safe by wrapping it into `with-mutex-lock`.) The defstruct macro returns the struct that is bound to `name.`

See also: [`new-struct`](#link6e65772d737472756374), [`make`](#link6d616b65), [`with-mutex-lock`](#link776974682d6d757465782d6c6f636b).	 [→index](#idx) [→topic](#oop)

## `defun` : macro/1 or more {#link646566756e}

Usage: `(defun ident (params ...) body ...)`

Define a function with name `ident`, a possibly empty list of `params`, and the remaining `body` expressions. This is a macro for (setq ident (lambda (params ...) body ...)) and binds the lambda-form to the given symbol. Like lambdas, the `params` of `defun` allow for a `&rest` keyword before the last parameter name. This binds all remaining arguments of a variadic function call to this parameter as a list.

See also: [`setq`](#link73657471), [`defmacro`](#link6465666d6163726f).	 [→index](#idx) [→topic](#lisp)

## `delete` : procedure/2 {#link64656c657465}

Usage: `(delete d key)`

Remove the value for `key` in dict `d`. This also removes the key.

See also: [`dict?`](#link646963743f), [`get`](#link676574), [`set`](#link736574).	 [→index](#idx) [→topic](#dict)

## `delete-zedit-all` : procedure/1 {#link64656c6574652d7a656469742d616c6c}

Usage: `(delete-zedit-all editor)`

Delete all text in the editor and corresponding tags. Tag stylers are preserved.

See also: [`delete-zedit`](#link64656c6574652d7a65646974).	 [→index](#idx) [→topic](#gui)

## `dequeue!` : macro/1 or more {#link6465717565756521}

Usage: `(dequeue! sym [def]) => any`

Get the next element from queue `sym`, which must be the unquoted name of a variable, and return it. If a default `def` is given, then this is returned if the queue is empty, otherwise nil is returned.

See also: [`make-queue`](#link6d616b652d7175657565), [`queue?`](#link71756575653f), [`enqueue!`](#link656e717565756521), [`glance`](#link676c616e6365), [`queue-empty?`](#link71756575652d656d7074793f), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx) [→topic](#data)

## `dict` : procedure/0 or more {#link64696374}

Usage: `(dict [li]) => dict`

Create a dictionary. The option `li` must be a list of the form '(key1 value1 key2 value2 ...). Dictionaries are unordered, hence also not sequences. Dictionaries are safe for concurrent access.

See also: [`array`](#link6172726179), [`list`](#link6c697374).	 [→index](#idx) [→topic](#dict)

## `dict->alist` : procedure/1 {#link646963742d3e616c697374}

Usage: `(dict->alist d) => li`

Convert a dictionary into an association list. Note that the resulting alist will be a set of proper pairs of the form '(a . b) if the values in the dictionary are not lists.

See also: [`dict`](#link64696374), [`dict-map`](#link646963742d6d6170), [`dict->list`](#link646963742d3e6c697374).	 [→index](#idx) [→topic](#conversion)

## `dict->array` : procedure/1 {#link646963742d3e6172726179}

Usage: `(dict-array d) => array`

Return an array that contains all key, value pairs of `d`. A key comes directly before its value, but otherwise the order is unspecified.

See also: [`dict->list`](#link646963742d3e6c697374), [`dict`](#link64696374).	 [→index](#idx) [→topic](#conversion)

## `dict->keys` : procedure/1 {#link646963742d3e6b657973}

Usage: `(dict->keys d) => li`

Return the keys of dictionary `d` in arbitrary order.

See also: [`dict`](#link64696374), [`dict->values`](#link646963742d3e76616c756573), [`dict->alist`](#link646963742d3e616c697374), [`dict->list`](#link646963742d3e6c697374).	 [→index](#idx) [→topic](#conversion)

## `dict->list` : procedure/1 {#link646963742d3e6c697374}

Usage: `(dict->list d) => li`

Return a list of the form '(key1 value1 key2 value2 ...), where the order of key, value pairs is unspecified.

See also: [`dict->array`](#link646963742d3e6172726179), [`dict`](#link64696374).	 [→index](#idx) [→topic](#conversion)

## `dict->values` : procedure/1 {#link646963742d3e76616c756573}

Usage: `(dict->values d) => li`

Return the values of dictionary `d` in arbitrary order.

See also: [`dict`](#link64696374), [`dict->keys`](#link646963742d3e6b657973), [`dict->alist`](#link646963742d3e616c697374), [`dict->list`](#link646963742d3e6c697374).	 [→index](#idx) [→topic](#conversion)

## `dict-copy` : procedure/1 {#link646963742d636f7079}

Usage: `(dict-copy d) => dict`

Return a copy of dict `d.`

See also: [`dict`](#link64696374), [`dict?`](#link646963743f).	 [→index](#idx) [→topic](#dict)

## `dict-empty?` : procedure/1 {#link646963742d656d7074793f}

Usage: `(dict-empty? d) => bool`

Return true if dict `d` is empty, nil otherwise. As crazy as this may sound, this can have O(n) complexity if the dict is not empty, but it is still going to be more efficient than any other method.

See also: [`dict`](#link64696374).	 [→index](#idx) [→topic](#dict)

## `dict-foreach` : procedure/2 {#link646963742d666f7265616368}

Usage: `(dict-foreach d proc)`

Call `proc` for side-effects with the key and value for each key, value pair in dict `d.`

See also: [`dict-map!`](#link646963742d6d617021), [`dict?`](#link646963743f), [`dict`](#link64696374).	 [→index](#idx) [→topic](#dict)

## `dict-map` : procedure/2 {#link646963742d6d6170}

Usage: `(dict-map dict proc) => dict`

Returns a copy of `dict` with `proc` applies to each key value pair as aruments. Keys are immutable, so `proc` must take two arguments and return the new value.

See also: [`dict-map!`](#link646963742d6d617021), [`map`](#link6d6170).	 [→index](#idx) [→topic](#dict)

## `dict-map!` : procedure/2 {#link646963742d6d617021}

Usage: `(dict-map! d proc)`

Apply procedure `proc` which takes the key and value as arguments to each key, value pair in dict `d` and set the respective value in `d` to the result of `proc`. Keys are not changed.

See also: [`dict`](#link64696374), [`dict?`](#link646963743f), [`dict-foreach`](#link646963742d666f7265616368).	 [→index](#idx) [→topic](#dict)

## `dict-merge` : procedure/2 {#link646963742d6d65726765}

Usage: `(dict-merge a b) => dict`

Create a new dict that contains all key-value pairs from dicts `a` and `b`. Note that this function is not symmetric. If a key is in both `a` and `b`, then the key value pair in `a` is retained for this key.

See also: [`dict`](#link64696374), [`dict-map`](#link646963742d6d6170), [`dict-map!`](#link646963742d6d617021), [`dict-foreach`](#link646963742d666f7265616368).	 [→index](#idx) [→topic](#dict)

## `dict-protect` : procedure/1 {#link646963742d70726f74656374}

Usage: `(dict-protect d)`

Protect dict `d` against changes. Attempting to set values in a protected dict will cause an error, but all values can be read and the dict can be copied. This function requires permission 'allow-protect.

See also: [`dict-unprotect`](#link646963742d756e70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`protected?`](#link70726f7465637465643f), [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f).	 [→index](#idx)

**Warning: Protected dicts are full readable and can be copied, so you may need to use protect to also prevent changes to the toplevel symbol storing the dict!** [→topic](#system)

## `dict-protected?` : procedure/1 {#link646963742d70726f7465637465643f}

Usage: `(dict-protected? d)`

Return true if the dict `d` is protected against mutation, nil otherwise.

See also: [`dict-protect`](#link646963742d70726f74656374), [`dict-unprotect`](#link646963742d756e70726f74656374), [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`protected?`](#link70726f7465637465643f), [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f).	 [→index](#idx) [→topic](#system)

## `dict-unprotect` : procedure/1 {#link646963742d756e70726f74656374}

Usage: `(dict-unprotect d)`

Unprotect the dict `d` so it can be mutated again. This function requires permission 'allow-unprotect.

See also: [`dict-protect`](#link646963742d70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`protected?`](#link70726f7465637465643f), [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f).	 [→index](#idx) [→topic](#system)

## `dict?` : procedure/1 {#link646963743f}

Usage: `(dict? obj) => bool`

Return true if `obj` is a dict, nil otherwise.

See also: [`dict`](#link64696374).	 [→index](#idx) [→topic](#dict)

## `dir` : procedure/1 {#link646972}

Usage: `(dir [path]) => li`

Obtain a directory list for `path`. If `path` is not specified, the current working directory is listed.

See also: [`dir?`](#link6469723f), [`open`](#link6f70656e), [`close`](#link636c6f7365), [`read`](#link72656164), [`write`](#link7772697465).	 [→index](#idx) [→topic](#fileio)

## `dir?` : procedure/1 {#link6469723f}

Usage: `(dir? path) => bool`

Check if the file at `path` is a directory and return true, nil if the file does not exist or is not a directory.

See also: [`file-exists?`](#link66696c652d6578697374733f), [`dir`](#link646972), [`open`](#link6f70656e), [`close`](#link636c6f7365), [`read`](#link72656164), [`write`](#link7772697465).	 [→index](#idx) [→topic](#fileio)

## `disable-object` : procedure/1 {#link64697361626c652d6f626a656374}

Usage: `(disable-object obj)`

Disable the canvas object `obj.`

See also: [`enable-object`](#link656e61626c652d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `div` : procedure/2 {#link646976}

Usage: `(div n k) => int`

Integer division of `n` by `k.`

See also: [`truncate`](#link7472756e63617465), [`/`](#link2f), [`int`](#link696e74).	 [→index](#idx) [→topic](#numeric)

## `dolist` : macro/1 or more {#link646f6c697374}

Usage: `(dolist (name list [result]) body ...) => li`

Traverse the list `list` in order, binding `name` to each element subsequently and evaluate the `body` expressions with this binding. The optional `result` is the result of the traversal, nil if it is not provided.

See also: [`letrec`](#link6c6574726563), [`foreach`](#link666f7265616368), [`map`](#link6d6170).	 [→index](#idx) [→topic](#lisp)

## `dotimes` : macro/1 or more {#link646f74696d6573}

Usage: `(dotimes (name count [result]) body ...) => any`

Iterate `count` times, binding `name` to the counter starting from 0 until the counter has reached count-1, and evaluate the `body` expressions each time with this binding. The optional `result` is the result of the iteration, nil if it is not provided.

See also: [`letrec`](#link6c6574726563), [`dolist`](#link646f6c697374), [`while`](#link7768696c65).	 [→index](#idx) [→topic](#lisp)

## `dump` : procedure/0 or more {#link64756d70}

Usage: `(dump [sym] [all?]) => li`

Return a list of symbols starting with the characters of `sym` or starting with any characters if `sym` is omitted, sorted alphabetically. When `all?` is true, then all symbols are listed, otherwise only symbols that do not contain "_" are listed. By convention, the underscore is used for auxiliary functions.

See also: [`dump-bindings`](#link64756d702d62696e64696e6773), [`save-zimage`](#link736176652d7a696d616765), [`load-zimage`](#link6c6f61642d7a696d616765).	 [→index](#idx) [→topic](#system)

## `dump-bindings` : procedure/0 {#link64756d702d62696e64696e6773}

Usage: `(dump-bindings) => li`

Return a list of all top-level symbols with bound values, including those intended for internal use.

See also: [`dump`](#link64756d70).	 [→index](#idx) [→topic](#system)

## `enable-object` : procedure/1 {#link656e61626c652d6f626a656374}

Usage: `(enable-object obj)`

Enable the canvas object `obj.`

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `enq` : procedure/1 {#link656e71}

Usage: `(enq proc)`

Put `proc` on a special internal queue for sequential execution and execute it when able. `proc` must be a prodedure that takes no arguments. The queue can be used to synchronizing i/o commands but special care must be taken that `proc` terminates, or else the system might be damaged.

See also: [`task`](#link7461736b), [`future`](#link667574757265), [`synout`](#link73796e6f7574), [`synouty`](#link73796e6f757479).	 [→index](#idx)

**Warning: Calls to enq can never be nested, neither explicitly or implicitly by calling enq anywhere else in the call chain!** [→topic](#concurrency)

## `enqueue!` : macro/2 {#link656e717565756521}

Usage: `(enqueue! sym elem)`

Put `elem` in queue `sym`, where `sym` is the unquoted name of a variable.

See also: [`make-queue`](#link6d616b652d7175657565), [`queue?`](#link71756575653f), [`dequeue!`](#link6465717565756521), [`glance`](#link676c616e6365), [`queue-empty?`](#link71756575652d656d7074793f), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx) [→topic](#data)

## `entry-accepts-tab?` : procedure/1 {#link656e7472792d616363657074732d7461623f}

Usage: `(entry-accepts-tab? entry) => bool`

Return `nil` when the entry does not accept tabs, `t` otherwise.

See also: [`new-entry`](#link6e65772d656e747279).	 [→index](#idx) [→topic](#gui)

## `epoch-ns->datelist` : procedure/1 {#link65706f63682d6e732d3e646174656c697374}

Usage: `(epoch-ns->datelist ns) => li`

Return the date list in UTC time corresponding to the Unix epoch nanoseconds `ns.`

See also: [`date->epoch-ns`](#link646174652d3e65706f63682d6e73), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`datestr`](#link64617465737472), [`datestr*`](#link646174657374722a), [`day-of-week`](#link6461792d6f662d7765656b), [`week-of-date`](#link7765656b2d6f662d64617465), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `eq?` : procedure/2 {#link65713f}

Usage: `(eq? x y) => bool`

Return true if `x` and `y` are equal, nil otherwise. In contrast to other LISPs, eq? checks for deep equality of arrays and dicts. However, lists are compared by checking whether they are the same cell in memory. Use `equal?` to check for deep equality of lists and other objects.

See also: [`equal?`](#link657175616c3f).	 [→index](#idx) [→topic](#lisp)

## `eql?` : procedure/2 {#link65716c3f}

Usage: `(eql? x y) => bool`

Returns true if `x` is equal to `y`, nil otherwise. This is currently the same as equal? but the behavior might change.

See also: [`equal?`](#link657175616c3f).	 [→index](#idx)

**Warning: Deprecated.** [→topic](#lisp)

## `equal?` : procedure/2 {#link657175616c3f}

Usage: `(equal? x y) => bool`

Return true if `x` and `y` are equal, nil otherwise. The equality is tested recursively for containers like lists and arrays.

See also: [`eq?`](#link65713f), [`eql?`](#link65716c3f).	 [→index](#idx) [→topic](#lisp)

## `error` : procedure/0 or more {#link6572726f72}

Usage: `(error [msgstr] [expr] ...)`

Raise an error, where `msgstr` and the optional expressions `expr`... work as in a call to fmt.

See also: [`fmt`](#link666d74), [`with-final`](#link776974682d66696e616c).	 [→index](#idx) [→topic](#system)

## `error->str` : procedure/1 {#link6572726f722d3e737472}

Usage: `(error->str datum) => str`

Convert a special error value to a string.

See also: [`*last-error*`](#link2a6c6173742d6572726f722a), [`error`](#link6572726f72), [`error?`](#link6572726f723f).	 [→index](#idx) [→topic](#system)

## `error?` : procedure/1 {#link6572726f723f}

Usage: `(error? datum) => bool`

Return true if `datum` is a special error value, nil otherwise.

See also: [`*last-error*`](#link2a6c6173742d6572726f722a), [`error->str`](#link6572726f722d3e737472), [`error`](#link6572726f72), [`eof?`](#link656f663f), [`valid?`](#link76616c69643f).	 [→index](#idx) [→topic](#system)

## `eval` : procedure/1 {#link6576616c}

Usage: `(eval expr) => any`

Evaluate the expression `expr` in the Z3S5 Machine Lisp interpreter and return the result. The evaluation environment is the system's environment at the time of the call.

See also: [`break`](#link627265616b), [`apply`](#link6170706c79).	 [→index](#idx) [→topic](#system)

## `even?` : procedure/1 {#link6576656e3f}

Usage: `(even? n) => bool`

Returns true if the integer `n` is even, nil if it is not even.

See also: [`odd?`](#link6f64643f).	 [→index](#idx) [→topic](#numeric)

## `exists?` : procedure/2 {#link6578697374733f}

Usage: `(exists? seq pred) => bool`

Return true if `pred` returns true for at least one element in sequence `seq`, nil otherwise.

See also: [`forall?`](#link666f72616c6c3f), [`list-exists?`](#link6c6973742d6578697374733f), [`array-exists?`](#link61727261792d6578697374733f), [`str-exists?`](#link7374722d6578697374733f), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#seq)

## `exit` : procedure/0 or more {#link65786974}

Usage: `(exit [n])`

Immediately shut down the system and return OS host error code `n`. The shutdown is performed gracefully and exit hooks are executed.

See also: .	 [→index](#idx) [→topic](#system)

## `expand-macros` : procedure/1 {#link657870616e642d6d6163726f73}

Usage: `(expand-macros expr) => expr`

Expands the macros in `expr`. This is an ordinary function and will not work on already compiled expressions such as a function bound to a symbol. However, it can be used to expand macros in expressions obtained by `read.`

See also: [`internalize`](#link696e7465726e616c697a65), [`externalize`](#link65787465726e616c697a65), [`load-library`](#link6c6f61642d6c696272617279).	 [→index](#idx) [→topic](#system)

## `expect` : macro/2 {#link657870656374}

Usage: `(expect value given)`

Registers a test under the current test name that checks that `value` is returned by `given`. The test is only executed when (run-selftest) is executed.

See also: [`expect-err`](#link6578706563742d657272), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374), [`testing`](#link74657374696e67).	 [→index](#idx) [→topic](#system)

## `expect-err` : macro/1 or more {#link6578706563742d657272}

Usage: `(expect-err expr ...)`

Registers a test under the current test name that checks that `expr` produces an error.

See also: [`expect`](#link657870656374), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374), [`testing`](#link74657374696e67).	 [→index](#idx) [→topic](#system)

## `expect-false` : macro/1 or more {#link6578706563742d66616c7365}

Usage: `(expect-false expr ...)`

Registers a test under the current test name that checks that `expr` is nil.

See also: [`expect`](#link657870656374), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374), [`testing`](#link74657374696e67).	 [→index](#idx) [→topic](#system)

## `expect-ok` : macro/1 or more {#link6578706563742d6f6b}

Usage: `(expect-ok expr ...)`

Registers a test under the current test name that checks that `expr` does not produce an error.

See also: [`expect`](#link657870656374), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374), [`testing`](#link74657374696e67).	 [→index](#idx) [→topic](#system)

## `expect-true` : macro/1 or more {#link6578706563742d74727565}

Usage: `(expect-true expr ...)`

Registers a test under the current test name that checks that `expr` is true (not nil).

See also: [`expect`](#link657870656374), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374), [`testing`](#link74657374696e67).	 [→index](#idx) [→topic](#system)

## `expr->str` : procedure/1 {#link657870722d3e737472}

Usage: `(expr->str expr) => str`

Convert a Lisp expression `expr` into a string. Does not use a stream port.

See also: [`str->expr`](#link7374722d3e65787072), [`str->expr*`](#link7374722d3e657870722a), [`openstr`](#link6f70656e737472), [`internalize`](#link696e7465726e616c697a65), [`externalize`](#link65787465726e616c697a65).	 [→index](#idx) [→topic](#conversion)

## `externalize` : procedure/1 {#link65787465726e616c697a65}

Usage: `(externalize sym [nonce]) => sexpr`

Obtain an external representation of top-level symbol `sym`. The optional `nonce` must be a value unique in each system zimage, in order to distinguish data from procedures.

See also: [`can-externalize?`](#link63616e2d65787465726e616c697a653f), [`externalize0`](#link65787465726e616c697a6530), [`current-zimage`](#link63757272656e742d7a696d616765), [`save-zimage`](#link736176652d7a696d616765), [`load-zimage`](#link6c6f61642d7a696d616765).	 [→index](#idx) [→topic](#system)

## `externalize0` : procedure/1 {#link65787465726e616c697a6530}

Usage: `(externalize0 arg) => any`

Attempts to externalize `arg` but falls back to the internal expression if `arg` cannot be externalized. This procedure never fails but `can-externalize?` may be false for the result. This function is only used in miscellaneous printing. Use `externalize` to externalize expressions for writing to disk.

See also: [`externalize`](#link65787465726e616c697a65), [`can-externalize?`](#link63616e2d65787465726e616c697a653f).	 [→index](#idx) [→topic](#system)

## `fdelete` : procedure/1 {#link6664656c657465}

Usage: `(fdelete path)`

Removes the file or directory at `path.`

See also: [`file-exists?`](#link66696c652d6578697374733f), [`dir?`](#link6469723f), [`dir`](#link646972).	 [→index](#idx)

**Warning: This function also deletes directories containing files and all of their subdirectories!** [→topic](#fileio)

## `feature?` : procedure/1 {#link666561747572653f}

Usage: `(feature? sym) => bool`

Return true if the Lisp feature identified by symbol `sym` is available, nil otherwise.

See also: [`*reflect*`](#link2a7265666c6563742a), [`on-feature`](#link6f6e2d66656174757265).	 [→index](#idx) [→topic](#system)

## `file-display-name` : procedure/1 {#link66696c652d646973706c61792d6e616d65}

Usage: `(file-display-name s) => str`

Return the name part of a file path without extension.

See also: [`file-name`](#link66696c652d6e616d65), [`file-ext`](#link66696c652d657874), [`file-path`](#link66696c652d70617468).	 [→index](#idx) [→topic](#system)

## `file-name` : procedure/1 {#link66696c652d6e616d65}

Usage: `(file-name s) => str`

Return the name part of a file path.

See also: [`file-display-name`](#link66696c652d646973706c61792d6e616d65), [`file-ext`](#link66696c652d657874), [`file-path`](#link66696c652d70617468).	 [→index](#idx) [→topic](#system)

## `file-path` : procedure/1 {#link66696c652d70617468}

Usage: `(file-path s) => str`

Return the directory part of a file path.

See also: [`file-name`](#link66696c652d6e616d65), [`file-ext`](#link66696c652d657874), [`file-display-name`](#link66696c652d646973706c61792d6e616d65), [`open`](#link6f70656e).	 [→index](#idx) [→topic](#system)

## `file-port?` : procedure/1 {#link66696c652d706f72743f}

Usage: `(file-port? p) => bool`

Return true if `p` is a file port, nil otherwise.

See also: [`port?`](#link706f72743f), [`str-port?`](#link7374722d706f72743f), [`open`](#link6f70656e), [`stropen`](#link7374726f70656e).	 [→index](#idx) [→topic](#fileio)

## `file-suffix` : procedure/1 {#link66696c652d737566666978}

Usage: `(file-suffix s) => str`

Return the suffix of a file path without including the dot.

See also: [`file-display-name`](#link66696c652d646973706c61792d6e616d65), [`file-name`](#link66696c652d6e616d65), [`file-path`](#link66696c652d70617468).	 [→index](#idx) [→topic](#system)

## `filter` : procedure/2 {#link66696c746572}

Usage: `(filter li pred) => li`

Return the list based on `li` with each element removed for which `pred` returns nil.

See also: [`list`](#link6c697374).	 [→index](#idx) [→topic](#lisp)

## `find-help-entries-with-incorrect-arity` : procedure/0 {#link66696e642d68656c702d656e74726965732d776974682d696e636f72726563742d6172697479}

Usage: `(find-help-entries-with-incorrect-arity) => li`

Return a list of help entries whose real arity in the system differs from the arity specified in the help system. That usually indicates that the arity in the help system is wrong.

See also: [`find-missing-help-entries`](#link66696e642d6d697373696e672d68656c702d656e7472696573), [`find-unneeded-help-entries`](#link66696e642d756e6e65656465642d68656c702d656e7472696573), [`sanity-check-help-system`](#link73616e6974792d636865636b2d68656c702d73797374656d).	 [→index](#idx)

## `find-missing-help-entries` : procedure/0 {#link66696e642d6d697373696e672d68656c702d656e7472696573}

Usage: `(find-missing-help-entries) => li`

Return a list of global symbols for which help entries are missing.

See also: [`dump`](#link64756d70), [`dump-bindings`](#link64756d702d62696e64696e6773), [`find-unneeded-help-entries`](#link66696e642d756e6e65656465642d68656c702d656e7472696573), [`find-help-entries-with-incorrect-arity`](#link66696e642d68656c702d656e74726965732d776974682d696e636f72726563742d6172697479).	 [→index](#idx) [→topic](#system)

## `find-unneeded-help-entries` : procedure/0 {#link66696e642d756e6e65656465642d68656c702d656e7472696573}

Usage: `(find-unneeded-help-entries) => li`

Return a list of help entries for which no symbols are defined.

See also: [`dump`](#link64756d70), [`dump-bindings`](#link64756d702d62696e64696e6773), [`find-missing-help-entries`](#link66696e642d6d697373696e672d68656c702d656e7472696573), [`find-help-entries-with-incorrect-arity`](#link66696e642d68656c702d656e74726965732d776974682d696e636f72726563742d6172697479).	 [→index](#idx)

**Warning: This function returns false positives! Special forms like setq and macro are listed even though they clearly are useful and should have a help entry.** [→topic](#system)

## `fl.abs` : procedure/1 {#link666c2e616273}

Usage: `(fl.abs x) => fl`

Return the absolute value of `x.`

See also: [`float`](#link666c6f6174), [`*`](#link2a).	 [→index](#idx) [→topic](#float)

## `fl.acos` : procedure/1 {#link666c2e61636f73}

Usage: `(fl.acos x) => fl`

Return the arc cosine of `x.`

See also: [`fl.cos`](#link666c2e636f73).	 [→index](#idx) [→topic](#float)

## `fl.asin` : procedure/1 {#link666c2e6173696e}

Usage: `(fl.asin x) => fl`

Return the arc sine of `x.`

See also: [`fl.acos`](#link666c2e61636f73).	 [→index](#idx) [→topic](#float)

## `fl.asinh` : procedure/1 {#link666c2e6173696e68}

Usage: `(fl.asinh x) => fl`

Return the inverse hyperbolic sine of `x.`

See also: [`fl.cosh`](#link666c2e636f7368).	 [→index](#idx) [→topic](#float)

## `fl.atan` : procedure/1 {#link666c2e6174616e}

Usage: `(fl.atan x) => fl`

Return the arctangent of `x` in radians.

See also: [`fl.atanh`](#link666c2e6174616e68), [`fl.tan`](#link666c2e74616e).	 [→index](#idx) [→topic](#float)

## `fl.atan2` : procedure/2 {#link666c2e6174616e32}

Usage: `(fl.atan2 x y) => fl`

Atan2 returns the arc tangent of `y` / `x`, using the signs of the two to determine the quadrant of the return value.

See also: [`fl.atan`](#link666c2e6174616e).	 [→index](#idx) [→topic](#float)

## `fl.atanh` : procedure/1 {#link666c2e6174616e68}

Usage: `(fl.atanh x) => fl`

Return the inverse hyperbolic tangent of `x.`

See also: [`fl.atan`](#link666c2e6174616e).	 [→index](#idx) [→topic](#float)

## `fl.cbrt` : procedure/1 {#link666c2e63627274}

Usage: `(fl.cbrt x) => fl`

Return the cube root of `x.`

See also: [`fl.sqrt`](#link666c2e73717274).	 [→index](#idx) [→topic](#float)

## `fl.ceil` : procedure/1 {#link666c2e6365696c}

Usage: `(fl.ceil x) => fl`

Round `x` up to the nearest integer, return it as a floating point number.

See also: [`fl.floor`](#link666c2e666c6f6f72), [`truncate`](#link7472756e63617465), [`int`](#link696e74), [`fl.round`](#link666c2e726f756e64), [`fl.trunc`](#link666c2e7472756e63).	 [→index](#idx) [→topic](#float)

## `fl.cos` : procedure/1 {#link666c2e636f73}

Usage: `(fl.cos x) => fl`

Return the cosine of `x.`

See also: [`fl.sin`](#link666c2e73696e).	 [→index](#idx) [→topic](#float)

## `fl.cosh` : procedure/1 {#link666c2e636f7368}

Usage: `(fl.cosh x) => fl`

Return the hyperbolic cosine of `x.`

See also: [`fl.cos`](#link666c2e636f73).	 [→index](#idx) [→topic](#float)

## `fl.dim` : procedure/2 {#link666c2e64696d}

Usage: `(fl.dim x y) => fl`

Return the maximum of x, y or 0.

See also: [`max`](#link6d6178).	 [→index](#idx) [→topic](#float)

## `fl.erf` : procedure/1 {#link666c2e657266}

Usage: `(fl.erf x) => fl`

Return the result of the error function of `x.`

See also: [`fl.erfc`](#link666c2e65726663), [`fl.dim`](#link666c2e64696d).	 [→index](#idx) [→topic](#float)

## `fl.erfc` : procedure/1 {#link666c2e65726663}

Usage: `(fl.erfc x) => fl`

Return the result of the complementary error function of `x.`

See also: [`fl.erfcinv`](#link666c2e65726663696e76), [`fl.erf`](#link666c2e657266).	 [→index](#idx) [→topic](#float)

## `fl.erfcinv` : procedure/1 {#link666c2e65726663696e76}

Usage: `(fl.erfcinv x) => fl`

Return the inverse of (fl.erfc `x`).

See also: [`fl.erfc`](#link666c2e65726663).	 [→index](#idx) [→topic](#float)

## `fl.erfinv` : procedure/1 {#link666c2e657266696e76}

Usage: `(fl.erfinv x) => fl`

Return the inverse of (fl.erf `x`).

See also: [`fl.erf`](#link666c2e657266).	 [→index](#idx) [→topic](#float)

## `fl.exp` : procedure/1 {#link666c2e657870}

Usage: `(fl.exp x) => fl`

Return e^`x`, the base-e exponential of `x.`

See also: [`fl.exp`](#link666c2e657870).	 [→index](#idx) [→topic](#float)

## `fl.exp2` : procedure/2 {#link666c2e65787032}

Usage: `(fl.exp2 x) => fl`

Return 2^`x`, the base-2 exponential of `x.`

See also: [`fl.exp`](#link666c2e657870).	 [→index](#idx) [→topic](#float)

## `fl.expm1` : procedure/1 {#link666c2e6578706d31}

Usage: `(fl.expm1 x) => fl`

Return e^`x-1`, the base-e exponential of (sub1 `x`). This is more accurate than (sub1 (fl.exp `x`)) when `x` is very small.

See also: [`fl.exp`](#link666c2e657870).	 [→index](#idx) [→topic](#float)

## `fl.floor` : procedure/1 {#link666c2e666c6f6f72}

Usage: `(fl.floor x) => fl`

Return `x` rounded to the nearest integer below as floating point number.

See also: [`fl.ceil`](#link666c2e6365696c), [`truncate`](#link7472756e63617465), [`int`](#link696e74).	 [→index](#idx) [→topic](#float)

## `fl.fma` : procedure/3 {#link666c2e666d61}

Usage: `(fl.fma x y z) => fl`

Return the fused multiply-add of `x`, `y`, `z`, which is `x` * `y` + `z.`

See also: [`*`](#link2a), [`+`](#link2b).	 [→index](#idx) [→topic](#float)

## `fl.frexp` : procedure/1 {#link666c2e6672657870}

Usage: `(fl.frexp x) => li`

Break `x` into a normalized fraction and an integral power of two. It returns a list of (frac exp) containing a float and an integer satisfying `x` == `frac` × 2^`exp` where the absolute value of `frac` is in the interval [0.5, 1).

See also: [`fl.exp`](#link666c2e657870).	 [→index](#idx) [→topic](#float)

## `fl.gamma` : procedure/1 {#link666c2e67616d6d61}

Usage: `(fl.gamma x) => fl`

Compute the Gamma function of `x.`

See also: [`fl.lgamma`](#link666c2e6c67616d6d61).	 [→index](#idx) [→topic](#float)

## `fl.hypot` : procedure/2 {#link666c2e6879706f74}

Usage: `(fl.hypot x y) => fl`

Compute the square root of x^2 and y^2.

See also: [`fl.sqrt`](#link666c2e73717274).	 [→index](#idx) [→topic](#float)

## `fl.ilogb` : procedure/1 {#link666c2e696c6f6762}

Usage: `(fl.ilogb x) => fl`

Return the binary exponent of `x` as a floating point number.

See also: [`fl.exp2`](#link666c2e65787032).	 [→index](#idx) [→topic](#float)

## `fl.inf` : procedure/1 {#link666c2e696e66}

Usage: `(fl.inf x) => fl`

Return positive 64 bit floating point infinity +INF if `x` >= 0 and negative 64 bit floating point finfinity -INF if `x` < 0.

See also: [`fl.is-nan?`](#link666c2e69732d6e616e3f).	 [→index](#idx) [→topic](#float)

## `fl.is-nan?` : procedure/1 {#link666c2e69732d6e616e3f}

Usage: `(fl.is-nan? x) => bool`

Return true if `x` is not a number according to IEEE 754 floating point arithmetics, nil otherwise.

See also: [`fl.inf`](#link666c2e696e66).	 [→index](#idx) [→topic](#float)

## `fl.j0` : procedure/1 {#link666c2e6a30}

Usage: `(fl.j0 x) => fl`

Apply the order-zero Bessel function of the first kind to `x.`

See also: [`fl.j1`](#link666c2e6a31), [`fl.jn`](#link666c2e6a6e), [`fl.y0`](#link666c2e7930), [`fl.y1`](#link666c2e7931), [`fl.yn`](#link666c2e796e).	 [→index](#idx) [→topic](#float)

## `fl.j1` : procedure/1 {#link666c2e6a31}

Usage: `(fl.j1 x) => fl`

Apply the the order-one Bessel function of the first kind `x.`

See also: [`fl.j0`](#link666c2e6a30), [`fl.jn`](#link666c2e6a6e), [`fl.y0`](#link666c2e7930), [`fl.y1`](#link666c2e7931), [`fl.yn`](#link666c2e796e).	 [→index](#idx) [→topic](#float)

## `fl.jn` : procedure/1 {#link666c2e6a6e}

Usage: `(fl.jn n x) => fl`

Apply the Bessel function of order `n` to `x`. The number `n` must be an integer.

See also: [`fl.j1`](#link666c2e6a31), [`fl.j0`](#link666c2e6a30), [`fl.y0`](#link666c2e7930), [`fl.y1`](#link666c2e7931), [`fl.yn`](#link666c2e796e).	 [→index](#idx) [→topic](#float)

## `fl.ldexp` : procedure/2 {#link666c2e6c64657870}

Usage: `(fl.ldexp x n) => fl`

Return the inverse of fl.frexp, `x` * 2^`n.`

See also: [`fl.frexp`](#link666c2e6672657870).	 [→index](#idx) [→topic](#float)

## `fl.lgamma` : procedure/1 {#link666c2e6c67616d6d61}

Usage: `(fl.lgamma x) => li`

Return a list containing the natural logarithm and sign (-1 or +1) of the Gamma function applied to `x.`

See also: [`fl.gamma`](#link666c2e67616d6d61).	 [→index](#idx) [→topic](#float)

## `fl.log` : procedure/1 {#link666c2e6c6f67}

Usage: `(fl.log x) => fl`

Return the natural logarithm of `x.`

See also: [`fl.log10`](#link666c2e6c6f673130), [`fl.log2`](#link666c2e6c6f6732), [`fl.logb`](#link666c2e6c6f6762), [`fl.log1p`](#link666c2e6c6f673170).	 [→index](#idx) [→topic](#float)

## `fl.log10` : procedure/1 {#link666c2e6c6f673130}

Usage: `(fl.log10 x) => fl`

Return the decimal logarithm of `x.`

See also: [`fl.log`](#link666c2e6c6f67), [`fl.log2`](#link666c2e6c6f6732), [`fl.logb`](#link666c2e6c6f6762), [`fl.log1p`](#link666c2e6c6f673170).	 [→index](#idx) [→topic](#float)

## `fl.log1p` : procedure/1 {#link666c2e6c6f673170}

Usage: `(fl.log1p x) => fl`

Return the natural logarithm of `x` + 1. This function is more accurate than (fl.log (add1 x)) if `x` is close to 0.

See also: [`fl.log`](#link666c2e6c6f67), [`fl.log2`](#link666c2e6c6f6732), [`fl.logb`](#link666c2e6c6f6762), [`fl.log10`](#link666c2e6c6f673130).	 [→index](#idx) [→topic](#float)

## `fl.log2` : procedure/1 {#link666c2e6c6f6732}

Usage: `(fl.log2 x) => fl`

Return the binary logarithm of `x`. This is important for calculating entropy, for example.

See also: [`fl.log`](#link666c2e6c6f67), [`fl.log10`](#link666c2e6c6f673130), [`fl.log1p`](#link666c2e6c6f673170), [`fl.logb`](#link666c2e6c6f6762).	 [→index](#idx) [→topic](#float)

## `fl.logb` : procedure/1 {#link666c2e6c6f6762}

Usage: `(fl.logb x) => fl`

Return the binary exponent of `x.`

See also: [`fl.log`](#link666c2e6c6f67), [`fl.log10`](#link666c2e6c6f673130), [`fl.log1p`](#link666c2e6c6f673170), [`fl.logb`](#link666c2e6c6f6762), [`fl.log2`](#link666c2e6c6f6732).	 [→index](#idx) [→topic](#float)

## `fl.max` : procedure/2 {#link666c2e6d6178}

Usage: `(fl.max x y) => fl`

Return the larger value of two floating point arguments `x` and `y.`

See also: [`fl.min`](#link666c2e6d696e), [`max`](#link6d6178), [`min`](#link6d696e).	 [→index](#idx) [→topic](#float)

## `fl.min` : procedure/2 {#link666c2e6d696e}

Usage: `(fl.min x y) => fl`

Return the smaller value of two floating point arguments `x` and `y.`

See also: [`fl.min`](#link666c2e6d696e), [`max`](#link6d6178), [`min`](#link6d696e).	 [→index](#idx) [→topic](#float)

## `fl.mod` : procedure/2 {#link666c2e6d6f64}

Usage: `(fl.mod x y) => fl`

Return the floating point remainder of `x` / `y.`

See also: [`fl.remainder`](#link666c2e72656d61696e646572).	 [→index](#idx) [→topic](#float)

## `fl.modf` : procedure/1 {#link666c2e6d6f6466}

Usage: `(fl.modf x) => li`

Return  integer and fractional floating-point numbers that sum to `x`. Both values have the same sign as `x.`

See also: [`fl.mod`](#link666c2e6d6f64).	 [→index](#idx) [→topic](#float)

## `fl.nan` : procedure/1 {#link666c2e6e616e}

Usage: `(fl.nan) => fl`

Return the IEEE 754 not-a-number value.

See also: [`fl.is-nan?`](#link666c2e69732d6e616e3f), [`fl.inf`](#link666c2e696e66).	 [→index](#idx) [→topic](#float)

## `fl.next-after` : procedure/1 {#link666c2e6e6578742d6166746572}

Usage: `(fl.next-after x) => fl`

Return the next representable floating point number after `x.`

See also: [`fl.is-nan?`](#link666c2e69732d6e616e3f), [`fl.nan`](#link666c2e6e616e), [`fl.inf`](#link666c2e696e66).	 [→index](#idx) [→topic](#float)

## `fl.pow` : procedure/2 {#link666c2e706f77}

Usage: `(fl.pow x y) => fl`

Return `x` to the power of `y` according to 64 bit floating point arithmetics.

See also: [`fl.pow10`](#link666c2e706f773130).	 [→index](#idx) [→topic](#float)

## `fl.pow10` : procedure/1 {#link666c2e706f773130}

Usage: `(fl.pow10 n) => fl`

Return 10 to the power of integer `n` as a 64 bit floating point number.

See also: [`fl.pow`](#link666c2e706f77).	 [→index](#idx) [→topic](#float)

## `fl.remainder` : procedure/2 {#link666c2e72656d61696e646572}

Usage: `(fl.remainder x y) => fl`

Return the IEEE 754 floating-point remainder of `x` / `y.`

See also: [`fl.mod`](#link666c2e6d6f64).	 [→index](#idx) [→topic](#float)

## `fl.round` : procedure/1 {#link666c2e726f756e64}

Usage: `(fl.round x) => fl`

Round `x` to the nearest integer floating point number according to floating point arithmetics.

See also: [`fl.round-to-even`](#link666c2e726f756e642d746f2d6576656e), [`fl.truncate`](#link666c2e7472756e63617465), [`int`](#link696e74), [`float`](#link666c6f6174).	 [→index](#idx) [→topic](#float)

## `fl.round-to-even` : procedure/1 {#link666c2e726f756e642d746f2d6576656e}

Usage: `(fl.round-to-even x) => fl`

Round `x` to the nearest even integer floating point number according to floating point arithmetics.

See also: [`fl.round`](#link666c2e726f756e64), [`fl.truncate`](#link666c2e7472756e63617465), [`int`](#link696e74), [`float`](#link666c6f6174).	 [→index](#idx) [→topic](#float)

## `fl.signbit` : procedure/1 {#link666c2e7369676e626974}

Usage: `(fl.signbit x) => bool`

Return true if `x` is negative, nil otherwise.

See also: [`fl.abs`](#link666c2e616273).	 [→index](#idx) [→topic](#float)

## `fl.sin` : procedure/1 {#link666c2e73696e}

Usage: `(fl.sin x) => fl`

Return the sine of `x.`

See also: [`fl.cos`](#link666c2e636f73).	 [→index](#idx) [→topic](#float)

## `fl.sinh` : procedure/1 {#link666c2e73696e68}

Usage: `(fl.sinh x) => fl`

Return the hyperbolic sine of `x.`

See also: [`fl.sin`](#link666c2e73696e).	 [→index](#idx) [→topic](#float)

## `fl.sqrt` : procedure/1 {#link666c2e73717274}

Usage: `(fl.sqrt x) => fl`

Return the square root of `x.`

See also: [`fl.pow`](#link666c2e706f77).	 [→index](#idx) [→topic](#float)

## `fl.tan` : procedure/1 {#link666c2e74616e}

Usage: `(fl.tan x) => fl`

Return the tangent of `x` in radian.

See also: [`fl.tanh`](#link666c2e74616e68), [`fl.sin`](#link666c2e73696e), [`fl.cos`](#link666c2e636f73).	 [→index](#idx) [→topic](#float)

## `fl.tanh` : procedure/1 {#link666c2e74616e68}

Usage: `(fl.tanh x) => fl`

Return the hyperbolic tangent of `x.`

See also: [`fl.tan`](#link666c2e74616e), [`flsinh`](#link666c73696e68), [`fl.cosh`](#link666c2e636f7368).	 [→index](#idx) [→topic](#float)

## `fl.trunc` : procedure/1 {#link666c2e7472756e63}

Usage: `(fl.trunc x) => fl`

Return the integer value of `x` as floating point number.

See also: [`truncate`](#link7472756e63617465), [`int`](#link696e74), [`fl.floor`](#link666c2e666c6f6f72).	 [→index](#idx) [→topic](#float)

## `fl.y0` : procedure/1 {#link666c2e7930}

Usage: `(fl.y0 x) => fl`

Return the order-zero Bessel function of the second kind applied to `x.`

See also: [`fl.y1`](#link666c2e7931), [`fl.yn`](#link666c2e796e), [`fl.j0`](#link666c2e6a30), [`fl.j1`](#link666c2e6a31), [`fl.jn`](#link666c2e6a6e).	 [→index](#idx) [→topic](#float)

## `fl.y1` : procedure/1 {#link666c2e7931}

Usage: `(fl.y1 x) => fl`

Return the order-one Bessel function of the second kind applied to `x.`

See also: [`fl.y0`](#link666c2e7930), [`fl.yn`](#link666c2e796e), [`fl.j0`](#link666c2e6a30), [`fl.j1`](#link666c2e6a31), [`fl.jn`](#link666c2e6a6e).	 [→index](#idx) [→topic](#float)

## `fl.yn` : procedure/1 {#link666c2e796e}

Usage: `(fl.yn n x) => fl`

Return the Bessel function of the second kind of order `n` applied to `x`. Argument `n` must be an integer value.

See also: [`fl.y0`](#link666c2e7930), [`fl.y1`](#link666c2e7931), [`fl.j0`](#link666c2e6a30), [`fl.j1`](#link666c2e6a31), [`fl.jn`](#link666c2e6a6e).	 [→index](#idx) [→topic](#float)

## `flatten` : procedure/1 {#link666c617474656e}

Usage: `(flatten lst) => list`

Flatten `lst`, making all elements of sublists elements of the flattened list.

See also: [`car`](#link636172), [`cdr`](#link636472), [`remove-duplicates`](#link72656d6f76652d6475706c696361746573).	 [→index](#idx) [→topic](#lisp)

## `float` : procedure/1 {#link666c6f6174}

Usage: `(float n) => float`

Convert `n` to a floating point value.

See also: [`int`](#link696e74).	 [→index](#idx) [→topic](#numeric)

## `fmt` : procedure/1 or more {#link666d74}

Usage: `(fmt s [args] ...) => str`

Format string `s` that contains format directives with arbitrary many `args` as arguments. The number of format directives must match the number of arguments. The format directives are the same as those for the esoteric and arcane programming language "Go", which was used on Earth for some time.

See also: [`out`](#link6f7574).	 [→index](#idx) [→topic](#str)

## `focus-canvas-object` : procedure/2 {#link666f6375732d63616e7661732d6f626a656374}

Usage: `(focus-canvas-object canvas object)`

Set the focus within `canvas` to `object`. The object must be a focusable canvas object such as an entry or button.

See also: [`focus-zedit`](#link666f6375732d7a65646974), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`get-focused-canvas-object`](#link6765742d666f63757365642d63616e7661732d6f626a656374), [`focus-next-canvas-object`](#link666f6375732d6e6578742d63616e7661732d6f626a656374), [`focus-previous-canvas-object`](#link666f6375732d70726576696f75732d63616e7661732d6f626a656374), [`unfocus-canvas-objects`](#link756e666f6375732d63616e7661732d6f626a65637473).	 [→index](#idx) [→topic](#gui)

## `focus-next-canvas-object` : procedure/1 {#link666f6375732d6e6578742d63616e7661732d6f626a656374}

Usage: `(focus-next-canvas-object canvas)`

Focus the next focusable user interface element in `canvas.`

See also: [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374), [`focus-previous-canvas-object`](#link666f6375732d70726576696f75732d63616e7661732d6f626a656374), [`unfocus-canvas-objects`](#link756e666f6375732d63616e7661732d6f626a65637473), [`get-focused-canvas-object`](#link6765742d666f63757365642d63616e7661732d6f626a656374).	 [→index](#idx) [→topic](#gui)

## `focus-previous-canvas-object` : procedure/1 {#link666f6375732d70726576696f75732d63616e7661732d6f626a656374}

Usage: `(focus-previous-canvas-object canvas)`

Focus the previous focusable user interface element in `canvas.`

See also: [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374), [`focus-next-canvas-object`](#link666f6375732d6e6578742d63616e7661732d6f626a656374), [`unfocus-canvas-objects`](#link756e666f6375732d63616e7661732d6f626a65637473), [`get-focused-canvas-object`](#link6765742d666f63757365642d63616e7661732d6f626a656374).	 [→index](#idx) [→topic](#gui)

## focus-zedit : nil {#link666f6375732d7a65646974}

Usage: `(focus-zedit editor)`

Set the focus to the given `editor`. This needs to be used instead of `focus-canvas-object` because a zedit is internally a complex widget and not a canvas object.

See also: [`new-zedit`](#link6e65772d7a65646974), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374).	 [→index](#idx) [→topic](#gui)

## `forall?` : procedure/2 {#link666f72616c6c3f}

Usage: `(forall? seq pred) => bool`

Return true if predicate `pred` returns true for all elements of sequence `seq`, nil otherwise.

See also: [`foreach`](#link666f7265616368), [`map`](#link6d6170), [`list-forall?`](#link6c6973742d666f72616c6c3f), [`array-forall?`](#link61727261792d666f72616c6c3f), [`str-forall?`](#link7374722d666f72616c6c3f), [`exists?`](#link6578697374733f), [`str-exists?`](#link7374722d6578697374733f), [`array-exists?`](#link61727261792d6578697374733f), [`list-exists?`](#link6c6973742d6578697374733f).	 [→index](#idx) [→topic](#seq)

## `force` : procedure/1 {#link666f726365}

Usage: `(force fut) => any`

Obtain the value of the computation encapsulated by future `fut`, halting the current task until it has been obtained. If the future never ends computation, e.g. in an infinite loop, the program may halt indefinitely.

See also: [`future`](#link667574757265), [`task`](#link7461736b), [`make-mutex`](#link6d616b652d6d75746578).	 [→index](#idx) [→topic](#concurrency)

## `foreach` : procedure/2 {#link666f7265616368}

Usage: `(foreach seq proc)`

Apply `proc` to each element of sequence `seq` in order, for the side effects.

See also: [`seq?`](#link7365713f), [`map`](#link6d6170).	 [→index](#idx) [→topic](#seq)

## `forget` : procedure/1 {#link666f72676574}

Usage: `(forget key)`

Forget the value associated with `key`. This permanently deletes the value from the persistent record.

See also: [`remember`](#link72656d656d626572), [`recall`](#link726563616c6c), [`recollect`](#link7265636f6c6c656374), [`recall-when`](#link726563616c6c2d7768656e), [`recall-info`](#link726563616c6c2d696e666f).	 [→index](#idx) [→topic](#db)

## `forget-gui-object` : procedure/1 {#link666f726765742d6775692d6f626a656374}

Usage: `(forget-gui-object int)`

Forget the GUI object `int`. This removes any association with the object but does not free internal resources if the object still exists. Internal use only.

See also: [`close-window`](#link636c6f73652d77696e646f77), [`close-gui`](#link636c6f73652d677569).	 [→index](#idx) [→topic](#gui)

## `functional-arity` : procedure/1 {#link66756e6374696f6e616c2d6172697479}

Usage: `(functional-arity proc) => int`

Return the arity of a functional `proc.`

See also: [`functional-arity*`](#link66756e6374696f6e616c2d61726974792a), [`functional?`](#link66756e6374696f6e616c3f), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx) [→topic](#system)

## `functional-arity*` : procedure/1 {#link66756e6374696f6e616c2d61726974792a}

Usage: `(functional-arity* proc) => int`

Return the extended arity of a functional `proc`, which is positive for fixed number of arguments like `functional-arity` and -1 for 0 or more arguments, -2 for 1 or more arguments, -3 for 2 or more arguments, and so on.

See also: [`functional-arity`](#link66756e6374696f6e616c2d6172697479), [`functional?`](#link66756e6374696f6e616c3f), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx) [→topic](#system)

## `functional-has-rest?` : procedure/1 {#link66756e6374696f6e616c2d6861732d726573743f}

Usage: `(functional-has-rest? proc) => bool`

Return true if the functional `proc` has a &rest argument, nil otherwise.

See also: [`functional?`](#link66756e6374696f6e616c3f), [`functional-arity`](#link66756e6374696f6e616c2d6172697479).	 [→index](#idx) [→topic](#system)

## `functional?` : macro/1 {#link66756e6374696f6e616c3f}

Usage: `(functional? arg) => bool`

Return true if `arg` is either a builtin function, a closure, or a macro, nil otherwise. This is the right predicate for testing whether the argument is applicable and has an arity.

See also: [`closure?`](#link636c6f737572653f), [`proc?`](#link70726f633f), [`functional-arity`](#link66756e6374696f6e616c2d6172697479), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx) [→topic](#system)

## `gensym` : procedure/0 {#link67656e73796d}

Usage: `(gensym) => sym`

Return a new symbol guaranteed to be unique during runtime.

See also: [`nonce`](#link6e6f6e6365).	 [→index](#idx) [→topic](#system)

## `get` : procedure/2 or more {#link676574}

Usage: `(get dict key [default]) => any`

Get the value for `key` in `dict`, return `default` if there is no value for `key`. If `default` is omitted, then nil is returned. Provide your own default if you want to store nil.

See also: [`dict`](#link64696374), [`dict?`](#link646963743f), [`set`](#link736574).	 [→index](#idx) [→topic](#dict)

## `get-action` : procedure/1 {#link6765742d616374696f6e}

Usage: `(get-action id) => action`

Return a cloned action based on `id` from the action registry. This action can be run using `action-start` and will get its own taskid.

See also: [`action`](#link616374696f6e), [`has-action-system?`](#link6861732d616374696f6e2d73797374656d3f), [`action-start`](#link616374696f6e2d7374617274), [`action-stop`](#link616374696f6e2d73746f70), [`register-action`](#link72656769737465722d616374696f6e).	 [→index](#idx) [→topic](#action)

## `get-clipboard-content` : procedure/0 {#link6765742d636c6970626f6172642d636f6e74656e74}

Usage: `(get-clipboard-content) => str`

Return the current content of the operating system clipboard as string. This function might raise an error if clipboard access is prohibited by host security settings.

See also: [`set-clipboard-content`](#link7365742d636c6970626f6172642d636f6e74656e74).	 [→index](#idx) [→topic](#gui)

## `get-device-info` : procedure/0 {#link6765742d6465766963652d696e666f}

Usage: `(get-device-info) => li`

Return a list with information about the current host device. This returns an association list where 'orientation might be one of '(vertical vertical-upside-down left right unknown), self-explanatory boolean keys 'is-mobile?, 'is-browser, 'has-keyboard?, and 'system-scale with the current scaling factor for graphics as float. The system scale is used to dynamically scale user interface elements to remain legible on hi res displays.

See also: [`close-gui`](#link636c6f73652d677569).	 [→index](#idx) [→topic](#gui)

## `get-entry-cursor` : procedure/1 {#link6765742d656e7472792d637572736f72}

Usage: `(get-entry-cursor entry) => sym`

Return a symbol that represents the current cursor of `entry`. Possible values are in '(default text crosshair pointer hresize vresize). Curiously, there is no way to set the cursor yet.

See also: [`new-entry`](#link6e65772d656e747279).	 [→index](#idx) [→topic](#gui)

## `get-entry-cursor-pos` : procedure/1 {#link6765742d656e7472792d637572736f722d706f73}

Usage: `(get-entry-cursor-pos entry) => li`

Return a list consisting of row number and column number of the current cursor position of the cursor in `entry.`

See also: [`set-entry-cursor-row`](#link7365742d656e7472792d637572736f722d726f77), [`set-entry-cursor-column`](#link7365742d656e7472792d637572736f722d636f6c756d6e).	 [→index](#idx) [→topic](#gui)

## `get-focused-canvas-object` : procedure/1 {#link6765742d666f63757365642d63616e7661732d6f626a656374}

Usage: `(get-focused-canvas-object canvas) => int`

Obtain the canvas object that is currently focused in `canvas`, or nil if there is none.

See also: [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374), [`focus-next-canvas-object`](#link666f6375732d6e6578742d63616e7661732d6f626a656374), [`focus-previous-canvas-object`](#link666f6375732d70726576696f75732d63616e7661732d6f626a656374).	 [→index](#idx) [→topic](#gui)

## `get-label-text` : procedure/1 {#link6765742d6c6162656c2d74657874}

Usage: `(get-label-text label) => str`

Gets the text of `label`

See also: [`set-label-text`](#link7365742d6c6162656c2d74657874), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `get-menu-item-label` : procedure/1 {#link6765742d6d656e752d6974656d2d6c6162656c}

Usage: `(get-menu-item-label item) => str`

Return the current label of the given menu `item.`

See also: [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`menu-item-disabled`](#link6d656e752d6974656d2d64697361626c6564), [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx) [→topic](#gui)

## `get-object-min-size` : procedure/1 {#link6765742d6f626a6563742d6d696e2d73697a65}

Usage: `(get-object-min-size obj) => li`

Return the minimum size of canvas object `obj` as a list containing the width and height as floats. The minimum size is computed based on various internal criteria and can only be changed for some special widgets.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `get-object-position` : procedure/1 {#link6765742d6f626a6563742d706f736974696f6e}

Usage: `(get-object-position obj) => li`

Return the position of canvas object `obj` as a list containing the x and y coordinates as floats.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `get-object-size` : procedure/1 {#link6765742d6f626a6563742d73697a65}

Usage: `(get-object-size obj) => li`

Return the size of canvas object `obj` as a list containing the width and height as floats.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `get-or-set` : procedure/3 {#link6765742d6f722d736574}

Usage: `(get-or-set d key value)`

Get the value for `key` in dict `d` if it already exists, otherwise set it to `value.`

See also: [`dict?`](#link646963743f), [`get`](#link676574), [`set`](#link736574).	 [→index](#idx) [→topic](#dict)

## `get-partitions` : procedure/2 {#link6765742d706172746974696f6e73}

Usage: `(get-partitions x n) => proc/1*`

Return an iterator procedure that returns lists of the form (start-offset end-offset bytes) with 0-index offsets for a given index `k`, or nil if there is no corresponding part, such that the sizes of the partitions returned in `bytes` summed up are `x` and and each partition is `n` or lower in size. The last partition will be the smallest partition with a `bytes` value smaller than `n` if `x` is not dividable without rest by `n`. If no argument is provided for the returned iterator, then it returns the number of partitions.

See also: [`nth-partition`](#link6e74682d706172746974696f6e), [`count-partitions`](#link636f756e742d706172746974696f6e73), [`get-file-partitions`](#link6765742d66696c652d706172746974696f6e73), [`iterate`](#link69746572617465).	 [→index](#idx) [→topic](#lisp)

## `get-progress-bar-value` : procedure/1 {#link6765742d70726f67726573732d6261722d76616c7565}

Usage: `(get-progress-bar-value bar) => num`

Return the current value of progress-bar `bar.`

See also: [`set-progress-bar`](#link7365742d70726f67726573732d626172), [`new-progress-bar`](#link6e65772d70726f67726573732d626172), [`new-infinite-progress-bar`](#link6e65772d696e66696e6974652d70726f67726573732d626172).	 [→index](#idx) [→topic](#gui)

## `get-scroll-offset` : procedure/1 {#link6765742d7363726f6c6c2d6f6666736574}

Usage: `(get-scroll-offset scroll) => li`

Get the offset of `scroll`, which may be a hscroll, vscroll, or scroll, as a position list of (x y) where `x` and `y` are floats.

See also: [`set-scroll-offset`](#link7365742d7363726f6c6c2d6f6666736574), [`new-scroll`](#link6e65772d7363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c), [`new-vscroll`](#link6e65772d767363726f6c6c).	 [→index](#idx) [→topic](#gui)

## `get-text-grid-cell` : procedure/3 {#link6765742d746578742d677269642d63656c6c}

Usage: `(get-text-grid-cell grid row column) => li`

Return the cell of `grid` at `row` and `column`. The result is a list consisting of a string containing one unicode rune and a grid style list. The style might be nil. If it is not nil, then the list contains a foreground and a background color list.

See also: [`get-text-grid-rune`](#link6765742d746578742d677269642d72756e65), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`get-text-grid-style`](#link6765742d746578742d677269642d7374796c65).	 [→index](#idx) [→topic](#gui)

## `get-text-grid-cell-size` : procedure/1 {#link6765742d746578742d677269642d63656c6c2d73697a65}

Usage: `(get-text-grid-cell-size grid) => li`

Return the size of one text grid cell as a list of floats (w h) where `w` is the width and `h` is the height.

See also: [`new-text-grid`](#link6e65772d746578742d67726964).	 [→index](#idx) [→topic](#gui)

## `get-text-grid-row` : procedure/2 {#link6765742d746578742d677269642d726f77}

Usage: `(get-text-grid-row grid row) => li`

Obtain a `row` of a text `grid`, where `row` is a 0-based index. This function returns a list of the form '(row style), where `style` is a grid style list and `row` is an array of lists consisting each of a unicode string containing one rune and a grid style list. Each entry of the `row` array represents an individual unicode glyph with a style, whereas the `style` list in the return argument represents an optional style of the whole row.

See also: [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874), [`remove-text-grid-row`](#link72656d6f76652d746578742d677269642d726f77), [`insert-text-grid-row`](#link696e736572742d746578742d677269642d726f77).	 [→index](#idx) [→topic](#gui)

## `get-text-grid-row-text` : procedure/2 {#link6765742d746578742d677269642d726f772d74657874}

Usage: `(get-text-grid-row-text grid row) => str`

Return the text of `row` in `grid` as a string without any style information.

See also: [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx) [→topic](#gui)

## `get-text-grid-rune` : procedure/3 {#link6765742d746578742d677269642d72756e65}

Usage: `(get-text-grid-rune grid row column) => str`

Return the string containing a single rune at the cell in `row` and `column` of `grid.`

See also: [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`get-text-grid-style`](#link6765742d746578742d677269642d7374796c65), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77).	 [→index](#idx) [→topic](#gui)

## `get-text-grid-tab-width` : procedure/1 {#link6765742d746578742d677269642d7461622d7769647468}

Usage: `(get-text-grid-tab-width grid) => int`

Return the current tabulator width of `grid` in space characters.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx) [→topic](#gui)

## `get-text-grid-text` : procedure/1 {#link6765742d746578742d677269642d74657874}

Usage: `(get-text-grid-text grid) => str`

Return the text of `grid` as a string without style information.

See also: [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`new-text-grid`](#link6e65772d746578742d67726964), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-rune`](#link6765742d746578742d677269642d72756e65), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c).	 [→index](#idx) [→topic](#gui)

## `get-window-canvas` : procedure/1 {#link6765742d77696e646f772d63616e766173}

Usage: `(get-window-canvas window) => int`

Get the canvas object of `window`, which is the area on which window elements are drawn. This is not the same as the window-content, which is a widget or other user interface element. The canvas is used for raw-drawing commands, for example for drawing circles and boxes. With a suitable layout that doesn't re-arrange objects, it can e.g. be used to draw overlays.

See also: [`get-window-content`](#link6765742d77696e646f772d636f6e74656e74), [`set-window-content`](#link7365742d77696e646f772d636f6e74656e74), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374).	 [→index](#idx) [→topic](#gui)

## `get-window-content` : procedure/2 {#link6765742d77696e646f772d636f6e74656e74}

Usage: `(get-window-content window) => int`

Get the canvas object ID that represents the main content of the window. This is usually a widget or a container with layout.

See also: [`set-window-content`](#link7365742d77696e646f772d636f6e74656e74), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173).	 [→index](#idx) [→topic](#gui)

## `get-window-icon` : procedure/1 {#link6765742d77696e646f772d69636f6e}

Usage: `(get-window-icon window) => int`

Obtain the icon ID of the icon of `window`. The resource obtained is not guaranteed to be a visible icon or might be a dummy, as not all windows have icons on all platforms.

See also: [`set-window-icon`](#link7365742d77696e646f772d69636f6e), [`new-icon`](#link6e65772d69636f6e), [`theme-icon`](#link7468656d652d69636f6e).	 [→index](#idx) [→topic](#gui)

## `get-window-main-menu` : procedure/1 {#link6765742d77696e646f772d6d61696e2d6d656e75}

Usage: `(get-window-main-menu window) => int`

Get the main menu ID of `window.`

See also: [`set-window-main-menu`](#link7365742d77696e646f772d6d61696e2d6d656e75), [`new-main-menu`](#link6e65772d6d61696e2d6d656e75).	 [→index](#idx) [→topic](#gui)

## `get-window-title` : procedure/1 {#link6765742d77696e646f772d7469746c65}

Usage: `(get-window-title window) => str`

Return the title of `window` as string.

See also: [`set-window-title`](#link7365742d77696e646f772d7469746c65).	 [→index](#idx) [→topic](#gui)

## `get-zedit-char-at` : procedure/2 {#link6765742d7a656469742d636861722d6174}

Usage: `(get-zedit-char-at zedit li) => str`

Return the character (unicode glyph) at position `li` in `zedit` as a string. The position `li` may contain a line, a line and column, or a line, column, and bool (the latter is ignored).

See also: [`get-zedit-text`](#link6765742d7a656469742d74657874), [`get-zedit-text-range`](#link6765742d7a656469742d746578742d72616e6765), [`get-zedit-next-pos`](#link6765742d7a656469742d6e6578742d706f73), [`get-zedit-prev-pos`](#link6765742d7a656469742d707265762d706f73), [`new-zedit`](#link6e65772d7a65646974), [`insert-zedit`](#link696e736572742d7a65646974), [`delete-zedit`](#link64656c6574652d7a65646974).	 [→index](#idx) [→topic](#gui)

## `get-zedit-columns` : procedure/1 {#link6765742d7a656469742d636f6c756d6e73}

Usage: `(get-zedit-columns editor) => int`

Get the number of columns in the editor. These are the columns that are displayed. If word wrapping is off, lines may have more columns.

See also: [`get-zedit-lines`](#link6765742d7a656469742d6c696e6573).	 [→index](#idx) [→topic](#gui)

## `get-zedit-config` : procedure/2 {#link6765742d7a656469742d636f6e666967}

Usage: `(get-zedit-config zedit sel) => any`

Set a configuration property of the given `zedit` to `b`. The selector `sel` can be one of the following: show-line-numbers? - show the line number display if true, show-whitespace? - show whitespace characters if true, line-wrap? - wrap lines to the size of the widget automatically, soft-wrap? - do not end lines in a hard line feed when line wrapping (default), draw-caret? - draw the caret if true, supress caret drawing if false, highlight-parens? - automatically highlight the matching opening paren while typing and moving the caret after a closing paren, highlight-paren-range? - automatically highlight the range of characters between the opening and closing paren when hightlight-parens? is true, get-word-at-left? - returns word left of caret if true, otherwise words are only triggered when the caret is on them, liberal-get-word-at? - words are parsed more like Z3S5 Lisp symbols, i.e., including all kinds of delimiters except for parentheses.

See also: [`new-zedit`](#link6e65772d7a65646974), [`set-zedit-config`](#link7365742d7a656469742d636f6e666967).	 [→index](#idx) [→topic](#gui)

## `get-zedit-last-column` : procedure/2 {#link6765742d7a656469742d6c6173742d636f6c756d6e}

Usage: `(get-zedit-last-column zedit line) => num`

Get the 0-based column number of the last column of the given `line` in editor `zedit.`

See also: [`new-zedit`](#link6e65772d7a65646974), [`get-zedit-last-line`](#link6765742d7a656469742d6c6173742d6c696e65).	 [→index](#idx) [→topic](#gui)

## `get-zedit-last-line` : procedure/1 {#link6765742d7a656469742d6c6173742d6c696e65}

Usage: `(get-zedit-last-line zedit) => num`

Get the 0-based line number of the last line in `zedit.`

See also: [`new-zedit`](#link6e65772d7a65646974), [`get-zedit-last-column`](#link6765742d7a656469742d6c6173742d636f6c756d6e).	 [→index](#idx)

## `get-zedit-lines` : procedure/1 {#link6765742d7a656469742d6c696e6573}

Usage: `(get-zedit-lines editor) => int`

Get the number of lines in the editor.

See also: [`get-zedit-columns`](#link6765742d7a656469742d636f6c756d6e73).	 [→index](#idx) [→topic](#gui)

## `get-zedit-next-pos` : procedure/2 {#link6765742d7a656469742d6e6578742d706f73}

Usage: `(get-zedit-next-pos zedit pos) => li`

Get the next position after `pos` in editor `zedit`. The returned list consists of a line, column, and a bool. The bool is true when the position is new and nil when `pos` is already the last position, in case of which the position returned is also the same as `pos.`

See also: [`get-zedit-prev-pos`](#link6765742d7a656469742d707265762d706f73), [`get-zedit-char-at`](#link6765742d7a656469742d636861722d6174).	 [→index](#idx) [→topic](#gui)

## `get-zedit-prev-pos` : procedure/2 {#link6765742d7a656469742d707265762d706f73}

Usage: `(get-zedit-prev-pos zedit pos) => li`

Get the previous position after `pos` in editor `zedit`. The returned list consists of a line, column, and a bool. The bool is true when the position is new and nil when position (0 0) has been reached, in case of which the position returned is (0 0 nil).

See also: [`get-zedit-next-pos`](#link6765742d7a656469742d6e6578742d706f73), [`get-zedit-char-at`](#link6765742d7a656469742d636861722d6174).	 [→index](#idx) [→topic](#gui)

## `get-zedit-text` : procedure/1 {#link6765742d7a656469742d74657874}

Usage: `(get-zedit-text zedit) => str`

Get the text in editor `zedit.`

See also: [`new-zedit`](#link6e65772d7a65646974), [`get-zedit-text`](#link6765742d7a656469742d74657874), [`get-zedit-text-range`](#link6765742d7a656469742d746578742d72616e6765), [`zedit-current-selection-text`](#link7a656469742d63757272656e742d73656c656374696f6e2d74657874).	 [→index](#idx) [→topic](#gui)

## `get-zedit-text-range` : procedure/2 {#link6765742d7a656469742d746578742d72616e6765}

Usage: `(get-zedit-text-range zedit interval) => str`

Obtain the text in `zedit` within range `interval` consisting of a list of two lists for the start and end position each. The function returns an empty string if there is no text at the interval.

See also: [`get-zedit-text`](#link6765742d7a656469742d74657874), [`zedit-current-selection-text`](#link7a656469742d63757272656e742d73656c656374696f6e2d74657874).	 [→index](#idx) [→topic](#gui)

## `get-zedit-top-line` : procedure/1 {#link6765742d7a656469742d746f702d6c696e65}

Usage: `(get-zedit-top-line zedit) => int`

Get the currently topmost visible line in the editor `zedit.`

See also: [`set-zedit-top-line`](#link7365742d7a656469742d746f702d6c696e65).	 [→index](#idx) [→topic](#gui)

## `getstacked` : procedure/3 {#link676574737461636b6564}

Usage: `(getstacked dict key default)`

Get the topmost element from the stack stored at `key` in `dict`. If the stack is empty or no stack is stored at key, then `default` is returned.

See also: [`pushstacked`](#link70757368737461636b6564), [`popstacked`](#link706f70737461636b6564).	 [→index](#idx) [→topic](#dict)

## `glance` : procedure/1 {#link676c616e6365}

Usage: `(glance s [def]) => any`

Peek the next element in a stack or queue without changing the data structure. If default `def` is provided, this is returned in case the stack or queue is empty; otherwise nil is returned.

See also: [`make-queue`](#link6d616b652d7175657565), [`make-stack`](#link6d616b652d737461636b), [`queue?`](#link71756575653f), [`enqueue?`](#link656e71756575653f), [`dequeue?`](#link646571756575653f), [`queue-len`](#link71756575652d6c656e), [`stack-len`](#link737461636b2d6c656e), [`pop!`](#link706f7021), [`push!`](#link7075736821).	 [→index](#idx) [→topic](#data)

## `global-startup-time` : procedure/0 {#link676c6f62616c2d737461727475702d74696d65}

Usage: `(global-startup-time) => num`

Return the global startup time in milliseconds. This is the time that the initial embedded init.lisp system required for booting, rounded to two decimal places unless it is 1 or less.

See also: [`now-ns`](#link6e6f772d6e73), [`time`](#link74696d65), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#system)

## `global-sym?` : procedure/1 {#link676c6f62616c2d73796d3f}

Usage: `(global-sym? sym) => bool`

Returns true if `sym` is a global symbol, nil otherwise. By convention, a symbol counts as global if it starts with a "*" character. This is used by library functions to determine whether a top-level symbol ought to be treated as local or global to the library.

See also: [`load`](#link6c6f6164), [`include`](#link696e636c756465), [`sym?`](#link73796d3f).	 [→index](#idx) [→topic](#lib)

## `gui` : macro/0 or more {#link677569}

Usage: `(gui body...)`

The `gui` macro ensures that all GUI function calls are executed in the OS main thread. Since the interpreter starts in its own thread, methods have to be embedded into `gui`, `gui+`, or gui* when they are called. Several GUI calls and other Lisp expressions can be put into the body of `gui` and will be called sequentially like in `progn`. The `gui` macros also need to be used whenever a GUI method is called from a task or future. See the GUI demo for examples of how to use them. When a GUI function is not embedded in one of the `gui` macros, a warning message is printed.

See also: [`gui+`](#link6775692b), [`gui*`](#link6775692a).	 [→index](#idx) [→topic](#gui)

## `gui*` : macro/0 or more {#link6775692a}

Usage: `(gui* body...) => any`

Like `gui`, but this macro blocks execution until all expressions in `body` have been evaluated in the main OS thread and returns the result of evaluating the last expression like `progn`. All GUI functions must be called with one of the `gui` macros, see the help entry for `gui` for more information.

See also: [`gui`](#link677569), [`gui*`](#link6775692a).	 [→index](#idx) [→topic](#gui)

## `gui+` : macro/0 or more {#link6775692b}

Usage: `(gui+ body...) => future`

Like `gui`, this macro ensures that all functions in the body are called in the OS main thread. All GUI functions must be called with one of the `gui` macros, see the help entry for `gui` for more information. `gui+` returns a future that represents the result of evaluating the last `body` expression like in `progn` but as a future.

See also: [`gui`](#link677569), [`gui*`](#link6775692a).	 [→index](#idx) [→topic](#gui)

## `has` : procedure/2 {#link686173}

Usage: `(has dict key) => bool`

Return true if the dict `dict` contains an entry for `key`, nil otherwise.

See also: [`dict`](#link64696374), [`get`](#link676574), [`set`](#link736574).	 [→index](#idx) [→topic](#dict)

## `has-action-system?` : procedure/0 {#link6861732d616374696f6e2d73797374656d3f}

Usage: `(has-action-system?) => bool`

This predicate is true if the action system is available, `false` otherwise.

See also: [`action`](#link616374696f6e), [`init-actions`](#link696e69742d616374696f6e73), [`action-start`](#link616374696f6e2d7374617274), [`action-stop`](#link616374696f6e2d73746f70), [`registered-actions`](#link726567697374657265642d616374696f6e73), [`register-action`](#link72656769737465722d616374696f6e).	 [→index](#idx) [→topic](#action)

## `has-action?` : procedure/1 {#link6861732d616374696f6e3f}

Usage: `(has-action? prefix name) => bool`

Return true if an action with the given `prefix` and `name` is registered, nil otherwise. Actions are indexed by id, so this is much slower than using `get-action` to retrieve a registered action by the value of the 'id property.

See also: [`get-action`](#link6765742d616374696f6e), [`action`](#link616374696f6e), [`has-action-system?`](#link6861732d616374696f6e2d73797374656d3f), [`register-action`](#link72656769737465722d616374696f6e).	 [→index](#idx) [→topic](#action)

## `has-key?` : procedure/2 {#link6861732d6b65793f}

Usage: `(has-key? d key) => bool`

Return true if `d` has key `key`, nil otherwise.

See also: [`dict?`](#link646963743f), [`get`](#link676574), [`set`](#link736574), [`delete`](#link64656c657465).	 [→index](#idx) [→topic](#dict)

## `has-method?` : procedure/2 {#link6861732d6d6574686f643f}

Usage: `(has-method? obj name) => bool`

Return true if `obj` has a method with name `name`, nil otherwise.

See also: [`defmethod`](#link6465666d6574686f64), [`has-prop?`](#link6861732d70726f703f), [`new`](#link6e6577), [`props`](#link70726f7073), [`methods`](#link6d6574686f6473), [`prop`](#link70726f70), [`setprop`](#link73657470726f70).	 [→index](#idx) [→topic](#oop)

## `has-prop?` : procedure/2 {#link6861732d70726f703f}

Usage: `(has-prop? obj slot) => bool`

Return true if `obj` has a property named `slot`, nil otherwise.

See also: [`has-method?`](#link6861732d6d6574686f643f), [`new`](#link6e6577), [`props`](#link70726f7073), [`methods`](#link6d6574686f6473), [`prop`](#link70726f70), [`setprop`](#link73657470726f70).	 [→index](#idx) [→topic](#oop)

## `help` : macro/1 {#link68656c70}

Usage: `(help sym)`

Display help information about `sym` (unquoted).

See also: [`defhelp`](#link64656668656c70), [`help-topics`](#link68656c702d746f70696373), [`help-about`](#link68656c702d61626f7574), [`help-topic-info`](#link68656c702d746f7069632d696e666f), [`set-help-topic-info`](#link7365742d68656c702d746f7069632d696e666f), [`help-entry`](#link68656c702d656e747279), [`*help*`](#link2a68656c702a), [`apropos`](#link6170726f706f73).	 [→index](#idx) [→topic](#help)

## help->manual-entry : nil {#link68656c702d3e6d616e75616c2d656e747279}

Usage: `(help->manual-entry key [level] [link?]) => str`

Looks up help for `key` and converts it to a manual section as markdown string. If there is no entry for `key`, then nil is returned. The optional `level` integer indicates the heading nesting. If `link?` is true an anchor is created for the key.

See also: [`help`](#link68656c70).	 [→index](#idx) [→topic](#help)

## `help-about` : procedure/1 or more {#link68656c702d61626f7574}

Usage: `(help-about topic [sel]) => li`

Obtain a list of symbols for which help about `topic` is available. If optional `sel` argument is left out or `any`, then any symbols with which the topic is associated are listed. If the optional `sel` argument is `first`, then a symbol is only listed if it has `topic` as first topic entry. This restricts the number of entries returned to a more essential selection.

See also: [`help-topics`](#link68656c702d746f70696373), [`help`](#link68656c70), [`apropos`](#link6170726f706f73).	 [→index](#idx) [→topic](#help)

## `help-entry` : procedure/1 {#link68656c702d656e747279}

Usage: `(help-entry sym) => list`

Get usage and help information for `sym.`

See also: [`defhelp`](#link64656668656c70), [`help`](#link68656c70), [`apropos`](#link6170726f706f73), [`*help*`](#link2a68656c702a), [`help-topics`](#link68656c702d746f70696373), [`help-about`](#link68656c702d61626f7574), [`set-help-topic-info`](#link7365742d68656c702d746f7069632d696e666f), [`help-topic-info`](#link68656c702d746f7069632d696e666f).	 [→index](#idx) [→topic](#help)

## `help-strings` : procedure/2 {#link68656c702d737472696e6773}

Usage: `(help-strings sym del) => li`

Obtain a string of help strings for a given symbol `sym`. The fields in the string are separated by string `del.`

See also: [`help`](#link68656c70), [`help-entry`](#link68656c702d656e747279), [`*help*`](#link2a68656c702a).	 [→index](#idx) [→topic](#help)

## `help-topic-info` : procedure/1 {#link68656c702d746f7069632d696e666f}

Usage: `(help-topic-info topic) => li`

Return a list containing a heading and an info string for help `topic`, or nil if no info is available.

See also: [`set-help-topic-info`](#link7365742d68656c702d746f7069632d696e666f), [`defhelp`](#link64656668656c70), [`help`](#link68656c70).	 [→index](#idx) [→topic](#help)

## `help-topics` : procedure/0 {#link68656c702d746f70696373}

Usage: `(help-topics) => li`

Obtain a list of help topics for commands.

See also: [`help`](#link68656c70), [`help-topic`](#link68656c702d746f706963), [`apropos`](#link6170726f706f73).	 [→index](#idx) [→topic](#help)

## `hex->blob` : procedure/1 {#link6865782d3e626c6f62}

Usage: `(hex->blob str) => blob`

Convert hex string `str` to a blob. This will raise an error if `str` is not a valid hex string.

See also: [`blob->hex`](#link626c6f622d3e686578), [`base64->blob`](#link6261736536342d3e626c6f62), [`ascii85->blob`](#link617363696938352d3e626c6f62), [`str->blob`](#link7374722d3e626c6f62).	 [→index](#idx) [→topic](#conversion)

## `hide-object` : procedure/1 {#link686964652d6f626a656374}

Usage: `(hide-object obj)`

Hide the canvas object `obj.`

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `hide-window` : procedure/1 {#link686964652d77696e646f77}

Usage: `(hide-window window)`

Hides `window`. It can be shown again using show-window.

See also: [`show-window`](#link73686f772d77696e646f77), [`close-window`](#link636c6f73652d77696e646f77).	 [→index](#idx) [→topic](#gui)

## `hook` : procedure/1 {#link686f6f6b}

Usage: `(hook symbol)`

Lookup the internal hook number from a symbolic name.

See also: [`*hooks*`](#link2a686f6f6b732a), [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b), [`remove-hooks`](#link72656d6f76652d686f6f6b73).	 [→index](#idx) [→topic](#system)

## `hour+` : procedure/2 {#link686f75722b}

Usage: `(hour+ dateli n) => dateli`

Adds `n` hours to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`minute+`](#link6d696e7574652b), [`day+`](#link6461792b), [`week+`](#link7765656b2b), [`month+`](#link6d6f6e74682b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `identity` : procedure/1 {#link6964656e74697479}

Usage: `(identity x)`

Return `x.`

See also: [`apply`](#link6170706c79), [`equal?`](#link657175616c3f).	 [→index](#idx) [→topic](#lisp)

## `if` : macro/3 {#link6966}

Usage: `(if cond expr1 expr2) => any`

Evaluate `expr1` if `cond` is true, otherwise evaluate `expr2.`

See also: [`cond`](#link636f6e64), [`when`](#link7768656e), [`unless`](#link756e6c657373).	 [→index](#idx) [→topic](#lisp)

## `inchars` : procedure/2 {#link696e6368617273}

Usage: `(inchars char chars) => bool`

Return true if char is in the charset chars, nil otherwise.

See also: [`chars`](#link6368617273), [`dict`](#link64696374), [`get`](#link676574), [`set`](#link736574), [`has`](#link686173).	 [→index](#idx) [→topic](#data)

## `include` : procedure/1 {#link696e636c756465}

Usage: `(include fi) => any`

Evaluate the lisp file `fi` one expression after the other in the current environment.

See also: [`read`](#link72656164), [`write`](#link7772697465), [`open`](#link6f70656e), [`close`](#link636c6f7365).	 [→index](#idx) [→topic](#system)

## `index` : procedure/2 or more {#link696e646578}

Usage: `(index seq elem [pred]) => int`

Return the first index of `elem` in `seq` going from left to right, using equality predicate `pred` for comparisons (default is eq?). If `elem` is not in `seq`, -1 is returned.

See also: [`nth`](#link6e7468), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#seq)

## `init-actions` : procedure/0 {#link696e69742d616374696f6e73}

Usage: `(init-actions)`

Initialize the action system, signals an error if the action system is not available.

See also: [`action`](#link616374696f6e), [`has-action-system?`](#link6861732d616374696f6e2d73797374656d3f), [`action-start`](#link616374696f6e2d7374617274), [`action-stop`](#link616374696f6e2d73746f70).	 [→index](#idx) [→topic](#action)

## `init-remember` : procedure/0 {#link696e69742d72656d656d626572}

Usage: `(init-remember)`

Initialize the remember database. This requires the modules 'kvdb and 'db enabled. The database is located at (str+ (sysdir 'z3s5-data) "/remembered.z3kv").

See also: [`remember`](#link72656d656d626572), [`recall-when`](#link726563616c6c2d7768656e), [`recall`](#link726563616c6c), [`forget`](#link666f72676574).	 [→index](#idx) [→topic](#db)

## `insert-text-grid-row` : procedure/2 {#link696e736572742d746578742d677269642d726f77}

Usage: `(insert-text-grid-row grid row)`

Insert a new text grid row before `row` in the given text `grid`. If `row` is the number of rows, a new row is appended to the end of the text grid.

See also: [`remove-text-grid-row`](#link72656d6f76652d746578742d677269642d726f77), [`count-text-grid-rows`](#link636f756e742d746578742d677269642d726f7773), [`new-text-grid`](#link6e65772d746578742d67726964), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77).	 [→index](#idx) [→topic](#gui)

## `instr` : procedure/2 {#link696e737472}

Usage: `(instr s1 s2) => int`

Return the index of the first occurrence of `s2` in `s1` (from left), or -1 if `s1` does not contain `s2.`

See also: [`str?`](#link7374723f), [`index`](#link696e646578).	 [→index](#idx) [→topic](#str)

## `int` : procedure/1 {#link696e74}

Usage: `(int n) => int`

Return `n` as an integer, rounding down to the nearest integer if necessary.

See also: [`float`](#link666c6f6174).	 [→index](#idx)

**Warning: If the number is very large this may result in returning the maximum supported integer number rather than the number as integer.** [→topic](#numeric)

## `intern` : procedure/1 {#link696e7465726e}

Usage: `(intern s) => sym`

Create a new interned symbol based on string `s.`

See also: [`gensym`](#link67656e73796d), [`str->sym`](#link7374722d3e73796d), [`make-symbol`](#link6d616b652d73796d626f6c).	 [→index](#idx) [→topic](#system)

## `internalize` : procedure/2 {#link696e7465726e616c697a65}

Usage: `(internalize arg nonce)`

Internalize an external representation of `arg`, using `nonce` for distinguishing between data and code that needs to be evaluated.

See also: [`externalize`](#link65787465726e616c697a65).	 [→index](#idx) [→topic](#system)

## `intrinsic` : procedure/1 {#link696e7472696e736963}

Usage: `(intrinsic sym) => any`

Attempt to obtain the value that is intrinsically bound to `sym`. Use this function to express the intention to use the pre-defined builtin value of a symbol in the base language.

See also: [`bind`](#link62696e64), [`unbind`](#link756e62696e64).	 [→index](#idx)

**Warning: This function currently only returns the binding but this behavior might change in future.** [→topic](#system)

## `intrinsic?` : procedure/1 {#link696e7472696e7369633f}

Usage: `(intrinsic? x) => bool`

Return true if `x` is an intrinsic built-in function, nil otherwise. Notice that this function tests the value and not that a symbol has been bound to the intrinsic.

See also: [`functional?`](#link66756e6374696f6e616c3f), [`macro?`](#link6d6163726f3f), [`closure?`](#link636c6f737572653f).	 [→index](#idx)

**Warning: What counts as an intrinsic or not may change from version to version. This is for internal use only.** [→topic](#system)

## `isa?` : procedure/2 {#link6973613f}

Usage: `(isa? obj class) => bool`

Return true if `obj` is an instance of `class`, nil otherwise.

See also: [`supers`](#link737570657273).	 [→index](#idx) [→topic](#oop)

## `iterate` : procedure/2 {#link69746572617465}

Usage: `(iterate it proc)`

Apply `proc` to each argument returned by iterator `it` in sequence, similar to the way foreach works. An iterator is a procedure that takes one integer as argument or no argument at all. If no argument is provided, the iterator returns the number of iterations. If an integer is provided, the iterator returns a non-nil value for the given index.

See also: [`foreach`](#link666f7265616368), [`get-partitions`](#link6765742d706172746974696f6e73).	 [→index](#idx) [→topic](#lisp)

## `kvdb.begin` : procedure/1 {#link6b7664622e626567696e}

Usage: `(kvdb.begin db)`

Begin a key-value database transaction. This can be committed by using kvdb.commit and rolled back by kvdb.rollback.

See also: [`kvdb.comit`](#link6b7664622e636f6d6974), [`kvdb.rollback`](#link6b7664622e726f6c6c6261636b).	 [→index](#idx)

**Warning: Transactions in key-value databases cannot be nested! You have to ensure that there is only one begin...commit pair.** [→topic](#db)

## `kvdb.close` : procedure/1 {#link6b7664622e636c6f7365}

Usage: `(kvdb.close db)`

Close a key-value db.

See also: [`kvdb.open`](#link6b7664622e6f70656e).	 [→index](#idx) [→topic](#db)

## `kvdb.commit` : procedure/1 {#link6b7664622e636f6d6d6974}

Usage: `(kvdb.commit db)`

Commit the current transaction, making any changes made since the transaction started permanent.

See also: [`kvdb.rollback`](#link6b7664622e726f6c6c6261636b), [`kvdb.begin`](#link6b7664622e626567696e).	 [→index](#idx) [→topic](#db)

## `kvdb.db?` : procedure/1 {#link6b7664622e64623f}

Usage: `(kvdb.db? datum) => bool`

Return true if the given datum is a key-value database, nil otherwise.

See also: [`kvdb.open`](#link6b7664622e6f70656e).	 [→index](#idx) [→topic](#db)

## `kvdb.forget` : procedure/1 {#link6b7664622e666f72676574}

Usage: `(kvdb.forget key)`

Forget the value for `key` if there is one.

See also: [`kvdb.set`](#link6b7664622e736574), [`kvdb.get`](#link6b7664622e676574).	 [→index](#idx) [→topic](#db)

## `kvdb.forget-everything` : procedure/1 {#link6b7664622e666f726765742d65766572797468696e67}

Usage: `(kvdb.forget-everything db)`

Erases all data from the given key-value database `db`, irrecoverably loosing ALL data in it.

See also: [`kvdb.forget`](#link6b7664622e666f72676574).	 [→index](#idx)

**Warning: This operation cannot be undone! Data for all types of keys is deleted. Permanent data loss is imminent!** [→topic](#db)

## `kvdb.get` : procedure/2 or more {#link6b7664622e676574}

Usage: `(kvdb.get db key [other]) => any`

Get the value stored at `key` in the key-value database `db`. If the value is found, it is returned. If the value is not found and `other` is specified, then `other` is returned. If the value is not found and `other` is not specified, then nil is returned.

See also: [`kvdb.set`](#link6b7664622e736574), [`kvdb.when`](#link6b7664622e7768656e), [`kvdb.info`](#link6b7664622e696e666f), [`kvdb.open`](#link6b7664622e6f70656e), [`kvdb.forget`](#link6b7664622e666f72676574), [`kvdb.close`](#link6b7664622e636c6f7365), [`kvdb.search`](#link6b7664622e736561726368), [`remember`](#link72656d656d626572), [`recall`](#link726563616c6c), [`forget`](#link666f72676574).	 [→index](#idx) [→topic](#db)

## `kvdb.info` : procedure/2 or more {#link6b7664622e696e666f}

Usage: `(db key [other]) => (str str)`

Return a list containing the info string and its fuzzy variant stored for `key` in `db`, `other` when the value for `key` is not found. The default for `other` is nil.

See also: [`kvdb.get`](#link6b7664622e676574), [`kvdb.when`](#link6b7664622e7768656e).	 [→index](#idx) [→topic](#db)

## `kvdb.open` : procedure/1 or more {#link6b7664622e6f70656e}

Usage: `(kvdb.open path) => kvdb-array`

Create or open a key-value database at `path.`

See also: [`kvdb.close`](#link6b7664622e636c6f7365).	 [→index](#idx) [→topic](#db)

## `kvdb.rollback` : procedure/1 {#link6b7664622e726f6c6c6261636b}

Usage: `(kvdb.rollback db)`

Rollback the changes made since the last transaction has been started and return the key-value database to its previous state.

See also: [`kvdb.commit`](#link6b7664622e636f6d6d6974), [`kvdb.begin`](#link6b7664622e626567696e).	 [→index](#idx) [→topic](#db)

## `kvdb.search` : procedure/2 or more {#link6b7664622e736561726368}

Usage: `(kvdb.search db s [keytype] [limit] [fuzzer]) => li`

Search the key-value database `db` for search expression string `s` for optional `keytype` and return a list of matching keys. The optional `keytype` may be one of '(all str sym int expr), where the default is 'all for any kind of key. If the optional `limit` is provided, then only `limit` entries are returned. Default limit is kvdb.*default-search-limit*. If `fuzzer` is a function provided, then a fuzzy string search is performed based on applying fuzzer to the search term; default is nil. 

See also: [`kvdb.get`](#link6b7664622e676574).	 [→index](#idx) [→topic](#db)

## `kvdb.set` : procedure/3 or more {#link6b7664622e736574}

Usage: `(kvdb.set db key value [info] [fuzzer])`

Set the `value` for `key` in key-value database `db`. The optional `info` string contains searchable information about the value that may be retrieved with the search function. The optional `fuzzer` must be a function that takes a string and yields a fuzzy variant of the string that can be used for fuzzy search. If no fuzzer is specified, then the default metaphone algorithm is used. Keys for the database must be externalizable but notice that integer keys may provide faster performance.

See also: [`kvdb.get`](#link6b7664622e676574), [`kvdb.forget`](#link6b7664622e666f72676574), [`kvdb.open`](#link6b7664622e6f70656e), [`kvdb.close`](#link6b7664622e636c6f7365), [`kvdb.search`](#link6b7664622e736561726368).	 [→index](#idx) [→topic](#db)

## `kvdb.when` : procedure/2 or more {#link6b7664622e7768656e}

Usage: `(kvdb.when db key [other]) => str`

Get the date in `db` when the entry for `key` was last modified as a date string. If there is no entry for `key`, then `other` is returned. If `other` is not specified and there is no `key`, then nil is returned.

See also: [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`kvdb.get`](#link6b7664622e676574), [`kvdb.info`](#link6b7664622e696e666f).	 [→index](#idx) [→topic](#db)

## `last` : procedure/1 or more {#link6c617374}

Usage: `(last seq [default]) => any`

Get the last element of sequence `seq` or return `default` if the sequence is empty. If `default` is not given and the sequence is empty, an error is raised.

See also: [`nth`](#link6e7468), [`nthdef`](#link6e7468646566), [`car`](#link636172), [`list-ref`](#link6c6973742d726566), [`array-ref`](#link61727261792d726566), [`string`](#link737472696e67), [`ref`](#link726566), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `lcons` : procedure/2 {#link6c636f6e73}

Usage: `(lcons datum li) => list`

Insert `datum` at the end of the list `li`. There may be a more efficient implementation of this in the future. Or, maybe not. Who knows?

See also: [`cons`](#link636f6e73), [`list`](#link6c697374), [`append`](#link617070656e64), [`nreverse`](#link6e72657665727365).	 [→index](#idx) [→topic](#lisp)

## `len` : procedure/1 {#link6c656e}

Usage: `(len seq) => int`

Return the length of `seq`. Works for lists, strings, arrays, and dicts.

See also: [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#seq)

## `let` : macro/1 or more {#link6c6574}

Usage: `(let args body ...) => any`

Bind each pair of symbol and expression in `args` and evaluate the expressions in `body` with these local bindings. Return the value of the last expression in `body.`

See also: [`letrec`](#link6c6574726563).	 [→index](#idx) [→topic](#lisp)

## `letrec` : macro/1 or more {#link6c6574726563}

Usage: `(letrec args body ...) => any`

Recursive let binds the symbol, expression pairs in `args` in a way that makes prior bindings available to later bindings and allows for recursive definitions in `args`, then evaluates the `body` expressions with these bindings.

See also: [`let`](#link6c6574).	 [→index](#idx) [→topic](#lisp)

## `lighten` : procedure/1 {#link6c69676874656e}

Usage: `(lighten color [amount]) => (r g b a)`

Return a lighter version of `color`. The optional positive `amount` specifies the amount of lightening (0-255).

See also: [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a), [`darken`](#link6461726b656e).	 [→index](#idx) [→topic](#ui)

## `ling.damerau-levenshtein` : procedure/2 {#link6c696e672e64616d657261752d6c6576656e73687465696e}

Usage: `(ling.damerau-levenshtein s1 s2) => num`

Compute the Damerau-Levenshtein distance between `s1` and `s2.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `ling.hamming` : procedure/2 {#link6c696e672e68616d6d696e67}

Usage: `(ling-hamming s1 s2) => num`

Compute the Hamming distance between `s1` and `s2.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `ling.jaro` : procedure/2 {#link6c696e672e6a61726f}

Usage: `(ling.jaro s1 s2) => num`

Compute the Jaro distance between `s1` and `s2.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `ling.jaro-winkler` : procedure/2 {#link6c696e672e6a61726f2d77696e6b6c6572}

Usage: `(ling.jaro-winkler s1 s2) => num`

Compute the Jaro-Winkler distance between `s1` and `s2.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `ling.levenshtein` : procedure/2 {#link6c696e672e6c6576656e73687465696e}

Usage: `(ling.levenshtein s1 s2) => num`

Compute the Levenshtein distance between `s1` and `s2.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `ling.match-rating-codex` : procedure/1 {#link6c696e672e6d617463682d726174696e672d636f646578}

Usage: `(ling.match-rating-codex s) => str`

Compute the Match-Rating-Codex of string `s.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `ling.match-rating-compare` : procedure/2 {#link6c696e672e6d617463682d726174696e672d636f6d70617265}

Usage: `(ling.match-rating-compare s1 s2) => bool`

Returns true if `s1` and `s2` are equal according to the Match-rating Comparison algorithm, nil otherwise.

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `ling.metaphone` : procedure/1 {#link6c696e672e6d65746170686f6e65}

Usage: `(ling.metaphone s) => str`

Compute the Metaphone representation of string `s.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `ling.nysiis` : procedure/1 {#link6c696e672e6e7973696973}

Usage: `(ling.nysiis s) => str`

Compute the Nysiis representation of string `s.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `ling.porter` : procedure/1 {#link6c696e672e706f72746572}

Usage: `(ling.porter s) => str`

Compute the stem of word string `s` using the Porter stemming algorithm.

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `ling.soundex` : procedure/1 {#link6c696e672e736f756e646578}

Usage: `(ling.soundex s) => str`

Compute the Soundex representation of string `s.`

See also: [`ling.match-rating-compare`](#link6c696e672e6d617463682d726174696e672d636f6d70617265), [`ling.levenshtein`](#link6c696e672e6c6576656e73687465696e), [`ling.jaro-winkler`](#link6c696e672e6a61726f2d77696e6b6c6572), [`ling.jaro`](#link6c696e672e6a61726f), [`ling.hamming`](#link6c696e672e68616d6d696e67), [`ling.damerau-levenshtein`](#link6c696e672e64616d657261752d6c6576656e73687465696e), [`ling.match-rating-codex`](#link6c696e672e6d617463682d726174696e672d636f646578), [`ling.porter`](#link6c696e672e706f72746572), [`ling.nysiis`](#link6c696e672e6e7973696973), [`ling.metaphone`](#link6c696e672e6d65746170686f6e65), [`ling.soundex`](#link6c696e672e736f756e646578).	 [→index](#idx) [→topic](#ling)

## `list` : procedure/0 or more {#link6c697374}

Usage: `(list [args] ...) => li`

Create a list from all `args`. The arguments must be quoted.

See also: [`cons`](#link636f6e73).	 [→index](#idx) [→topic](#lisp)

## `list->array` : procedure/1 {#link6c6973742d3e6172726179}

Usage: `(list->array li) => array`

Convert the list `li` to an array.

See also: [`list`](#link6c697374), [`array`](#link6172726179), [`string`](#link737472696e67), [`nth`](#link6e7468), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#conversion)

## `list->set` : procedure/1 {#link6c6973742d3e736574}

Usage: `(list->set li) => dict`

Create a dict containing true for each element of list `li.`

See also: [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty`](#link7365742d656d707479).	 [→index](#idx) [→topic](#conversion)

## `list->str` : procedure/1 {#link6c6973742d3e737472}

Usage: `(list->str li) => string`

Return the string that is composed out of the chars in list `li.`

See also: [`array->str`](#link61727261792d3e737472), [`str->list`](#link7374722d3e6c697374), [`chars`](#link6368617273).	 [→index](#idx) [→topic](#conversion)

## `list-exists?` : procedure/2 {#link6c6973742d6578697374733f}

Usage: `(list-exists? li pred) => bool`

Return true if `pred` returns true for at least one element in list `li`, nil otherwise.

See also: [`exists?`](#link6578697374733f), [`forall?`](#link666f72616c6c3f), [`array-exists?`](#link61727261792d6578697374733f), [`str-exists?`](#link7374722d6578697374733f), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#lisp)

## `list-forall?` : procedure/2 {#link6c6973742d666f72616c6c3f}

Usage: `(list-all? li pred) => bool`

Return true if predicate `pred` returns true for all elements of list `li`, nil otherwise.

See also: [`foreach`](#link666f7265616368), [`map`](#link6d6170), [`forall?`](#link666f72616c6c3f), [`array-forall?`](#link61727261792d666f72616c6c3f), [`str-forall?`](#link7374722d666f72616c6c3f), [`exists?`](#link6578697374733f).	 [→index](#idx) [→topic](#lisp)

## `list-foreach` : procedure/2 {#link6c6973742d666f7265616368}

Usage: `(list-foreach li proc)`

Apply `proc` to each element of list `li` in order, for the side effects.

See also: [`mapcar`](#link6d6170636172), [`map`](#link6d6170), [`foreach`](#link666f7265616368).	 [→index](#idx) [→topic](#lisp)

## `list-last` : procedure/1 {#link6c6973742d6c617374}

Usage: `(list-last li) => any`

Return the last element of `li.`

See also: [`reverse`](#link72657665727365), [`nreverse`](#link6e72657665727365), [`car`](#link636172), [`1st`](#link317374), [`last`](#link6c617374).	 [→index](#idx) [→topic](#lisp)

## `list-ref` : procedure/2 {#link6c6973742d726566}

Usage: `(list-ref li n) => any`

Return the element with index `n` of list `li`. Lists are 0-indexed.

See also: [`array-ref`](#link61727261792d726566), [`nth`](#link6e7468).	 [→index](#idx) [→topic](#lisp)

## `list-reverse` : procedure/1 {#link6c6973742d72657665727365}

Usage: `(list-reverse li) => li`

Create a reversed copy of `li.`

See also: [`reverse`](#link72657665727365), [`array-reverse`](#link61727261792d72657665727365), [`str-reverse`](#link7374722d72657665727365).	 [→index](#idx) [→topic](#lisp)

## `list-slice` : procedure/3 {#link6c6973742d736c696365}

Usage: `(list-slice li low high) => li`

Return the slice of the list `li` starting at index `low` (inclusive) and ending at index `high` (exclusive).

See also: [`slice`](#link736c696365), [`array-slice`](#link61727261792d736c696365).	 [→index](#idx) [→topic](#lisp)

## `list-uri` : procedure/1 {#link6c6973742d757269}

Usage: `(list-uri uri) => vec`

List a listable URI string `uri` if possible. If the URI string is not valid or valid but not listable, an error occurs. A listable URI is a file URI pointing to a folder, for example.

See also: [`show-folder-open`](#link73686f772d666f6c6465722d6f70656e), [`listable-uri?`](#link6c69737461626c652d7572693f), [`uri?`](#link7572693f).	 [→index](#idx) [→topic](#gui)

## `list?` : procedure/1 {#link6c6973743f}

Usage: `(list? obj) => bool`

Return true if `obj` is a list, nil otherwise.

See also: [`cons?`](#link636f6e733f), [`atom?`](#link61746f6d3f), [`null?`](#link6e756c6c3f).	 [→index](#idx) [→topic](#lisp)

## `listable-uri?` : procedure/1 {#link6c69737461626c652d7572693f}

Usage: `(listable-uri? s) => bool`

Return true if the string `s` represents a listable URI such as one pointing to a folder on the filesystem; otherwise, nil is returned.

See also: [`uri?`](#link7572693f), [`list-uri`](#link6c6973742d757269).	 [→index](#idx) [→topic](#gui)

## `load` : procedure/1 or more {#link6c6f6164}

Usage: `(load prefix [fi])`

Loads the Lisp file at `fi` as a library or program with the given `prefix`. If only a prefix is specified, load attempts to find a corresponding file at path (str+ (sysdir 'z3s5-data) "/prg/prefix/prefix.lisp"). Loading binds all non-global toplevel symbols of the definitions in file `fi` to the form prefix.symbol and replaces calls to them in the definitions appropriately. Symbols starting with "*" such as *cancel* are not modified. To give an example, if `fi` contains a definition (defun bar ...) and the prefix is 'foo, then the result of the import is equivalent to (defun foo.bar ...), and so on for any other definitions. The importer preorder-traverses the source and looks for setq and lambdas after macro expansion has taken place. By convention, the entry point of executable programs is a function (run) so the loaded program can be executed with the command (prefix.run).

See also: [`include`](#link696e636c756465), [`global-sym?`](#link676c6f62616c2d73796d3f).	 [→index](#idx) [→topic](#lib)

## `load-zimage` : procedure/1 or more {#link6c6f61642d7a696d616765}

Usage: `(load-zimage fi) => li`

Load the zimage file `fi`, if possible, and return a list containing information about the zimage after it has been loaded. If the zimage fails the semantic version check, then an error is raised.

See also: [`save-zimage`](#link736176652d7a696d616765), [`run-zimage`](#link72756e2d7a696d616765), [`zimage-loadable?`](#link7a696d6167652d6c6f616461626c653f).	 [→index](#idx) [→topic](#zimage)

## `macro?` : procedure/1 {#link6d6163726f3f}

Usage: `(macro? x) => bool`

Return true if `x` is a macro, nil otherwise.

See also: [`functional?`](#link66756e6374696f6e616c3f), [`intrinsic?`](#link696e7472696e7369633f), [`closure?`](#link636c6f737572653f), [`functional-arity`](#link66756e6374696f6e616c2d6172697479), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx) [→topic](#system)

## `make` : macro/2 {#link6d616b65}

Usage: `(make name props)`

Create a new record (struct instance) of struct `name` (unquoted) with properties `props`. Each clause in `props` must be a list of property name and initial value.

See also: [`make*`](#link6d616b652a), [`defstruct`](#link646566737472756374).	 [→index](#idx) [→topic](#oop)

## `make*` : macro/1 or more {#link6d616b652a}

Usage: `(make* name prop1 ...)`

Create a new record (struct instance) of struct `name` (unquoted) with property clauses `prop-1` ... `prop-n`, where each clause is a list of property name and initial value like in `make.`

See also: [`make`](#link6d616b65), [`defstruct`](#link646566737472756374).	 [→index](#idx) [→topic](#oop)

## `make-blob` : procedure/1 {#link6d616b652d626c6f62}

Usage: `(make-blob n) => blob`

Make a binary blob of size `n` initialized to zeroes.

See also: [`blob-free`](#link626c6f622d66726565), [`valid?`](#link76616c69643f), [`blob-equal?`](#link626c6f622d657175616c3f).	 [→index](#idx) [→topic](#binary)

## `make-mutex` : procedure/1 {#link6d616b652d6d75746578}

Usage: `(make-mutex) => mutex`

Create a new mutex.

See also: [`mutex-lock`](#link6d757465782d6c6f636b), [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`mutex-rlock`](#link6d757465782d726c6f636b), [`mutex-runlock`](#link6d757465782d72756e6c6f636b).	 [→index](#idx) [→topic](#concurrency)

## `make-or-get-zedit-style-tag` : procedure/4 {#link6d616b652d6f722d6765742d7a656469742d7374796c652d746167}

Usage: `(make-or-get-zedit-style-tag editor style draw-full-line?) => int`

Create a new tag or get an existing tag for marking the style of text. The style must be an a-list containing with boolean keys in '(bold italic monospace) followed by nil or non-nil, or keys 'text-color or 'background-color followed by a color list with 16-bit r g b a values. If `draw-full-line?` is not nil, then the full line is drawn (like in a selection) when multiple lines are drawn; otherwise, only actual text is drawn with the color and not whitespace at the end of a line. The function returns a tag ID and automatically adds the corresponding style function to render the text in the given color.

See also: [`new-zedit`](#link6e65772d7a65646974), [`color64->color`](#link636f6c6f7236342d3e636f6c6f72), [`color->color64`](#link636f6c6f722d3e636f6c6f723634), [`theme-color`](#link7468656d652d636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a).	 [→index](#idx) [→topic](#gui)

## `make-queue` : procedure/0 {#link6d616b652d7175657565}

Usage: `(make-queue) => array`

Make a synchronized queue.

See also: [`queue?`](#link71756575653f), [`enqueue!`](#link656e717565756521), [`dequeue!`](#link6465717565756521), [`glance`](#link676c616e6365), [`queue-empty?`](#link71756575652d656d7074793f), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx)

**Warning: Never change the array of a synchronized data structure directly, or your warranty is void!** [→topic](#data)

## `make-set` : procedure/0 or more {#link6d616b652d736574}

Usage: `(make-set [arg1] ... [argn]) => dict`

Create a dictionary out of arguments `arg1` to `argn` that stores true for very argument.

See also: [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx) [→topic](#data)

## `make-stack` : procedure/0 {#link6d616b652d737461636b}

Usage: `(make-stack) => array`

Make a synchronized stack.

See also: [`stack?`](#link737461636b3f), [`push!`](#link7075736821), [`pop!`](#link706f7021), [`stack-empty?`](#link737461636b2d656d7074793f), [`stack-len`](#link737461636b2d6c656e), [`glance`](#link676c616e6365).	 [→index](#idx)

**Warning: Never change the array of a synchronized data structure directly, or your warranty is void!** [→topic](#data)

## `make-symbol` : procedure/1 {#link6d616b652d73796d626f6c}

Usage: `(make-symbol s) => sym`

Create a new symbol based on string `s.`

See also: [`str->sym`](#link7374722d3e73796d).	 [→index](#idx) [→topic](#system)

## `map` : procedure/2 {#link6d6170}

Usage: `(map seq proc) => seq`

Return the copy of `seq` that is the result of applying `proc` to each element of `seq.`

See also: [`seq?`](#link7365713f), [`mapcar`](#link6d6170636172), [`strmap`](#link7374726d6170).	 [→index](#idx) [→topic](#seq)

## `map-pairwise` : procedure/2 {#link6d61702d7061697277697365}

Usage: `(map-pairwise seq proc) => seq`

Applies `proc` in order to subsequent pairs in `seq`, assembling the sequence that results from the results of `proc`. Function `proc` takes two arguments and must return a proper list containing two elements. If the number of elements in `seq` is odd, an error is raised.

See also: [`map`](#link6d6170).	 [→index](#idx) [→topic](#seq)

## `mapcar` : procedure/2 {#link6d6170636172}

Usage: `(mapcar li proc) => li`

Return the list obtained from applying `proc` to each elements in `li.`

See also: [`map`](#link6d6170), [`foreach`](#link666f7265616368).	 [→index](#idx) [→topic](#lisp)

## `max` : procedure/1 or more {#link6d6178}

Usage: `(max x1 x2 ...) => num`

Return the maximum of the given numbers.

See also: [`min`](#link6d696e), [`minmax`](#link6d696e6d6178).	 [→index](#idx) [→topic](#numeric)

## `member` : procedure/2 {#link6d656d626572}

Usage: `(member key li) => li`

Return the cdr of `li` starting with `key` if `li` contains an element equal? to `key`, nil otherwise.

See also: [`assoc`](#link6173736f63), [`equal?`](#link657175616c3f).	 [→index](#idx) [→topic](#lisp)

## `memq` : procedure/2 {#link6d656d71}

Usage: `(memq key li)`

Return the cdr of `li` starting with `key` if `li` contains an element eq? to `key`, nil otherwise.

See also: [`member`](#link6d656d626572), [`eq?`](#link65713f).	 [→index](#idx) [→topic](#lisp)

## `memstats` : procedure/0 {#link6d656d7374617473}

Usage: `(memstats) => dict`

Return a dict with detailed memory statistics for the system.

See also: [`collect-garbage`](#link636f6c6c6563742d67617262616765).	 [→index](#idx) [→topic](#system)

## `menu-item-checked?` : procedure/1 {#link6d656e752d6974656d2d636865636b65643f}

Usage: `(menu-item-checked? item) => bool`

Return true if `item` is currently checked, nil otherwise.

See also: [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`menu-item-disabled?`](#link6d656e752d6974656d2d64697361626c65643f), [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx) [→topic](#gui)

## `menu-item-disabled?` : procedure/1 {#link6d656e752d6974656d2d64697361626c65643f}

Usage: `(menu-item-disabled? item) => bool`

Return true if `item` is currently disabled, nil otherwise.

See also: [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx) [→topic](#gui)

## `methods` : procedure/1 {#link6d6574686f6473}

Usage: `(methods obj) => li`

Return the list of methods of `obj`, which must be a class, object, or class name.

See also: [`has-method?`](#link6861732d6d6574686f643f), [`new`](#link6e6577), [`props`](#link70726f7073), [`prop`](#link70726f70), [`setprop`](#link73657470726f70), [`has-prop?`](#link6861732d70726f703f).	 [→index](#idx) [→topic](#oop)

## `min` : procedure/1 or more {#link6d696e}

Usage: `(min x1 x2 ...) => num`

Return the minimum of the given numbers.

See also: [`max`](#link6d6178), [`minmax`](#link6d696e6d6178).	 [→index](#idx) [→topic](#numeric)

## `minmax` : procedure/3 {#link6d696e6d6178}

Usage: `(minmax pred li acc) => any`

Go through `li` and test whether for each `elem` the comparison (pred elem acc) is true. If so, `elem` becomes `acc`. Once all elements of the list have been compared, `acc` is returned. This procedure can be used to implement generalized minimum or maximum procedures.

See also: [`min`](#link6d696e), [`max`](#link6d6178).	 [→index](#idx) [→topic](#numeric)

## `minute+` : procedure/2 {#link6d696e7574652b}

Usage: `(minute+ dateli n) => dateli`

Adds `n` minutes to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`hour+`](#link686f75722b), [`day+`](#link6461792b), [`week+`](#link7765656b2b), [`month+`](#link6d6f6e74682b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `mod` : procedure/2 {#link6d6f64}

Usage: `(mod x y) => num`

Compute `x` modulo `y.`

See also: [`%`](#link25), [`/`](#link2f).	 [→index](#idx) [→topic](#numeric)

## `month+` : procedure/2 {#link6d6f6e74682b}

Usage: `(month+ dateli n) => dateli`

Adds `n` months to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`minute+`](#link6d696e7574652b), [`hour+`](#link686f75722b), [`day+`](#link6461792b), [`week+`](#link7765656b2b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `move-object` : procedure/2 {#link6d6f76652d6f626a656374}

Usage: `(move-object obj position)`

Move the canvas object `obj` to the given `position` list, containing its `x` and `y` coordinates as floats.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `mutex-lock` : procedure/1 {#link6d757465782d6c6f636b}

Usage: `(mutex-lock m)`

Lock the mutex `m` for writing. This may halt the current task until the mutex has been unlocked by another task.

See also: [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`make-mutex`](#link6d616b652d6d75746578), [`mutex-rlock`](#link6d757465782d726c6f636b), [`mutex-runlock`](#link6d757465782d72756e6c6f636b).	 [→index](#idx) [→topic](#concurrency)

## `mutex-rlock` : procedure/1 {#link6d757465782d726c6f636b}

Usage: `(mutex-rlock m)`

Lock the mutex `m` for reading. This will allow other tasks to read from it, too, but may block if another task is currently locking it for writing.

See also: [`mutex-runlock`](#link6d757465782d72756e6c6f636b), [`mutex-lock`](#link6d757465782d6c6f636b), [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`make-mutex`](#link6d616b652d6d75746578).	 [→index](#idx) [→topic](#concurrency)

## `mutex-runlock` : procedure/1 {#link6d757465782d72756e6c6f636b}

Usage: `(mutex-runlock m)`

Unlock the mutex `m` from reading.

See also: [`mutex-lock`](#link6d757465782d6c6f636b), [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`mutex-rlock`](#link6d757465782d726c6f636b), [`make-mutex`](#link6d616b652d6d75746578).	 [→index](#idx) [→topic](#concurrency)

## `mutex-unlock` : procedure/1 {#link6d757465782d756e6c6f636b}

Usage: `(mutex-unlock m)`

Unlock the mutex `m` for writing. This releases ownership of the mutex and allows other tasks to lock it for writing.

See also: [`mutex-lock`](#link6d757465782d6c6f636b), [`make-mutex`](#link6d616b652d6d75746578), [`mutex-rlock`](#link6d757465782d726c6f636b), [`mutex-runlock`](#link6d757465782d72756e6c6f636b).	 [→index](#idx) [→topic](#concurrency)

## `nconc` : procedure/0 or more {#link6e636f6e63}

Usage: `(nconc li1 li2 ...) => li`

Concatenate `li1`, `li2`, and so forth, like with append, but destructively modifies `li1.`

See also: [`append`](#link617070656e64).	 [→index](#idx) [→topic](#lisp)

## `new` : macro/1 or more {#link6e6577}

Usage: `(new class [props] ...)`

Create a new object of class `class` with initial property bindings `props` clauses as remaining arguments. Each `props` clause must be a list of the form (sym value), where `sym` is a symbol and `value` is evaluated first before it is assigned to `sym.`

See also: [`defclass`](#link646566636c617373).	 [→index](#idx) [→topic](#oop)

## `new-app-tabs` : procedure/0 or more {#link6e65772d6170702d74616273}

Usage: `(new-app-tabs tab-item ...) => int`

Create a new application tabs, which allow users to choose different items within an application.

See also: [`new-doc-tabs`](#link6e65772d646f632d74616273), [`new-tabitem`](#link6e65772d7461626974656d), [`new-tabitem-with-icon`](#link6e65772d7461626974656d2d776974682d69636f6e).	 [→index](#idx) [→topic](#gui)

## `new-border` : procedure/4 or more {#link6e65772d626f72646572}

Usage: `(new-border top bottom left right [obj ...]) => int`

Create a new border layout, which is one of the most useful layouts. Any of `top`, `bottom`, `left`, and `right` is put in the respective place (with minimum size) and might also be `nil` for no widget. The remaining canvas objects `obj` are arranged in the center and take maximum size. This allows you e.g. to put a list on the left side of a window, a panel of buttons on the top, and the main content in another container in the center.

See also: [`new-container`](#link6e65772d636f6e7461696e6572), [`new-container-without-layout`](#link6e65772d636f6e7461696e65722d776974686f75742d6c61796f7574), [`new-vscroll`](#link6e65772d767363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c).	 [→index](#idx) [→topic](#gui)

## `new-button` : procedure/2 {#link6e65772d627574746f6e}

Usage: `(new-button label proc) => int`

Return a new button with the given `label` and without an icon. The callback `proc` is called without arguments when the button is pressed or tapped.

See also: [`new-button-with-icon`](#link6e65772d627574746f6e2d776974682d69636f6e), [`new-hyperlink`](#link6e65772d68797065726c696e6b), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `new-button-with-icon` : procedure/3 {#link6e65772d627574746f6e2d776974682d69636f6e}

Usage: `(new-button-with-icon label icon proc) => int`

Return a new button the given `label` and `icon`. The callback `proc` is called without arguments when the button is pressed.

See also: [`new-button`](#link6e65772d627574746f6e), [`new-icon`](#link6e65772d69636f6e), [`theme-icon`](#link7468656d652d69636f6e).	 [→index](#idx) [→topic](#gui)

## `new-center-layout` : procedure/0 {#link6e65772d63656e7465722d6c61796f7574}

Usage: `(new-center-layout) => int`

Create a new center layout, which centers container elements (possibly overlapping). This may be used for drawing centered on the window, for example.

See also: [`new-form`](#link6e65772d666f726d), [`append-form`](#link617070656e642d666f726d), [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx) [→topic](#gui)

## `new-check` : procedure/2 {#link6e65772d636865636b}

Usage: `(new-check title proc) => int`

Create and return a new check box with the given `title` string and a callback procedure `proc`. The callback `proc` is called with the new state of the check box as bool when it has changed.

See also: [`new-choice`](#link6e65772d63686f696365).	 [→index](#idx) [→topic](#gui)

## `new-choice` : procedure/3 {#link6e65772d63686f696365}

Usage: `(new-choice selector string-list proc) => int`

Create and return a new choice representing choices in `string-list`. If `selector` is 'radio-group, a group of radio buttons is created with options in `string-list`. If `selector` is 'select, a more compact selection menu is created with the options in `string-list`. The callback `proc` takes a string that represents the choice that has been selected.

See also: [`set-select-options`](#link7365742d73656c6563742d6f7074696f6e73), [`new-check`](#link6e65772d636865636b).	 [→index](#idx) [→topic](#gui)

## `new-circle` : procedure/1 or more {#link6e65772d636972636c65}

Usage: `(new-circle fill-color [pos1] [pos2] [stroke-color] [stroke-width]) => int`

Draw and return a circle with the given NRGBA `fill-color`. If the optional `pos1` and `pos2` position lists of `x` and `y` coordinates in floats are given , then the circle is drawn inside the rectangle defined by these positions. The optional `stroke-color` and `stroke-width` arguments determine the outline of the circle. Notice that circle's size and position may be set by the layout of the container, so to set these manually using `pos1` and `pos2` you need to make sure the underlying container has no such layout.

See also: [`new-rectangle`](#link6e65772d72656374616e676c65), [`new-line-new-text`](#link6e65772d6c696e652d6e65772d74657874).	 [→index](#idx) [→topic](#gui)

## `new-combined-string-validator` : procedure/1 or more {#link6e65772d636f6d62696e65642d737472696e672d76616c696461746f72}

Usage: `(new-combined-string-validator validator-1 [...validator-n]) => int`

Combine validators `validator-1` to `validator-n` into a combined string validator and return it.

See also: [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72), [`new-validator`](#link6e65772d76616c696461746f72), [`new-regexp-validator`](#link6e65772d7265676578702d76616c696461746f72), [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72), [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b), [`validate-object`](#link76616c69646174652d6f626a656374).	 [→index](#idx) [→topic](#gui)

## `new-container` : procedure/1 or more {#link6e65772d636f6e7461696e6572}

Usage: `(new-container layout obj ...) => int`

Create a new container with the given `layout` and various canvas objects `obj` arranged by the layout.

See also: [`new-container-without-layout`](#link6e65772d636f6e7461696e65722d776974686f75742d6c61796f7574), [`new-border`](#link6e65772d626f72646572), [`new-vscroll`](#link6e65772d767363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c).	 [→index](#idx) [→topic](#gui)

## `new-container-without-layout` : procedure/0 or more {#link6e65772d636f6e7461696e65722d776974686f75742d6c61796f7574}

Usage: `(new-container-without-layout obj ...) => int`

Create a new container without a layout (overlapping objects) with the given canvas objects `obj.`

See also: [`new-container`](#link6e65772d636f6e7461696e6572), [`new-border`](#link6e65772d626f72646572).	 [→index](#idx) [→topic](#gui)

## `new-doc-tabs` : procedure/0 or more {#link6e65772d646f632d74616273}

Usage: `(new-doc-tabs tab-item ...) => int`

Create new document tabs, which allow users to choose different items in a window (not the application as a whole like app-tabs).

See also: [`new-app-tabs`](#link6e65772d6170702d74616273), [`new-tabitem`](#link6e65772d7461626974656d), [`new-tabitem-with-icon`](#link6e65772d7461626974656d2d776974682d69636f6e).	 [→index](#idx) [→topic](#gui)

## `new-entry` : procedure/1 {#link6e65772d656e747279}

Usage: `(new-entry [selector]) => int`

Create a new text entry field based on the optional `selector` symbol. `selector` can be a symbol in '(single-line multi-line password). The default is 'single-line.

See also: [`set-entry-on-change-callback`](#link7365742d656e7472792d6f6e2d6368616e67652d63616c6c6261636b), [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72), [`entry-accepts-tab?`](#link656e7472792d616363657074732d7461623f), [`get-entry-cursor-pos`](#link6765742d656e7472792d637572736f722d706f73), [`set-entry-cursor-row`](#link7365742d656e7472792d637572736f722d726f77), [`set-entry-cursor-column`](#link7365742d656e7472792d637572736f722d636f6c756d6e), [`set-entry-on-cursor-change-callback`](#link7365742d656e7472792d6f6e2d637572736f722d6368616e67652d63616c6c6261636b), [`get-entry-cursor`](#link6765742d656e7472792d637572736f72), [`get-entry-selected-text`](#link6765742d656e7472792d73656c65637465642d74657874), [`set-entry-min-rows-visible`](#link7365742d656e7472792d6d696e2d726f77732d76697369626c65), [`set-entry-place-holder`](#link7365742d656e7472792d706c6163652d686f6c646572), [`set-entry-text`](#link7365742d656e7472792d74657874).	 [→index](#idx) [→topic](#gui)

## `new-form` : procedure/0 {#link6e65772d666f726d}

Usage: `(new-form)`

Return a new form container, which orders widgets in rows, where each row has a label and a widget whose columns are aligned with the other rows. Use append-form to add label and widgets.

See also: [`append-form`](#link617070656e642d666f726d).	 [→index](#idx) [→topic](#gui)

## `new-form-item` : procedure/3 {#link6e65772d666f726d2d6974656d}

Usage: `(new-form-item text widget hint-text) => int`

Create a new form item. Such an item can be used in `show-form` dialogs to display forms. For adding items to a form based on `new-form`, use `append-form` to append text and strings to forms instead.

See also: [`show-form`](#link73686f772d666f726d).	 [→index](#idx) [→topic](#gui)

## `new-form-layout` : procedure/0 {#link6e65772d666f726d2d6c61796f7574}

Usage: `(new-form-layout) => int`

Create a form layout, which arranges elements in two columns per row, where the columns are aligned.

See also: [`new-form`](#link6e65772d666f726d), [`append-form`](#link617070656e642d666f726d), [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx) [→topic](#gui)

## `new-grid-layout` : procedure/1 {#link6e65772d677269642d6c61796f7574}

Usage: `(new-grid-layout n) => int`

Create a new grid layout, which arranges elements in `n` columns.

See also: [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx) [→topic](#gui)

## `new-grid-wrap-layout` : procedure/2 {#link6e65772d677269642d777261702d6c61796f7574}

Usage: `(new-grid-wrap-layout width height) => int`

Create a new grid wrap layout, which arranges elements such that each element has the given `width` and `height`, and wraps lines based on the size of the parent container.

See also: [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx) [→topic](#gui)

## `new-hbox-layout` : procedure/0 {#link6e65772d68626f782d6c61796f7574}

Usage: `(new-hbox-layout) => int`

Create a new horizontal box layout, which lays out container elements horizontally.

See also: [`new-spacer`](#link6e65772d737061636572), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx) [→topic](#gui)

## `new-hscroll` : procedure/1 {#link6e65772d687363726f6c6c}

Usage: `(new-hscroll obj) => int`

Embed canvas object `obj` into a new horizontal scroll container, which allows the user to scroll horizontally if `obj` does not fit into the hscroll container horizontally.

See also: [`new-scroll`](#link6e65772d7363726f6c6c), [`new-vscroll`](#link6e65772d767363726f6c6c), [`new-container`](#link6e65772d636f6e7461696e6572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574).	 [→index](#idx) [→topic](#gui)

## `new-hsplit` : procedure/2 {#link6e65772d6873706c6974}

Usage: `(new-hsplit obj1 obj2) => int`

Return a new horizontal divider between canvas object `obj1` and `obj2`. The user can adjust the division by drag & drop.

See also: [`set-split-offset`](#link7365742d73706c69742d6f6666736574), [`new-vsplit`](#link6e65772d7673706c6974).	 [→index](#idx) [→topic](#gui)

## `new-hyperlink` : procedure/2 {#link6e65772d68797065726c696e6b}

Usage: `(new-hyperlink label url) => int`

Create a new hyperlink with given `label` string and an `url` string as embedded link. A hyperlink looks like a label with link style; when it is clicked, the `url` is called by the default operating system mechanism for opening urls. Using hyperlinks might be disallowed by the host system configuration HyperlinksAllowed and may be re-written by the host system using the CheckHyperlinks function. If HyperlinksAllowed is false in the active GUI config of the host, this function raises an error. It also parses the given `url` and will raise an error if it does not represent a valid URL.

See also: [`new-button`](#link6e65772d627574746f6e), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx)

**Warning: Allowing the host to open hyperlinks usually launches a web browser and the details depend on the operating system. There is an added security risk!** [→topic](#gui)

## `new-icon` : procedure/1 {#link6e65772d69636f6e}

Usage: `(new-icon resource) => int`

Create a new icon from `resource`, which must be suitable to create an image.

See also: [`theme-icon`](#link7468656d652d69636f6e).	 [→index](#idx) [→topic](#gui)

## `new-image-from-file` : procedure/1 {#link6e65772d696d6167652d66726f6d2d66696c65}

Usage: `(new-image-from-file path) => int`

Create and return a new image from the image file at `path`, which must be a PNG file.

See also: [`new-image-from-resource`](#link6e65772d696d6167652d66726f6d2d7265736f75726365).	 [→index](#idx) [→topic](#gui)

## `new-image-from-resource` : procedure/1 {#link6e65772d696d6167652d66726f6d2d7265736f75726365}

Usage: `(new-image-from-resource resource) => int`

Create and return a new image from the given `resource.`

See also: [`new-image-from-file`](#link6e65772d696d6167652d66726f6d2d66696c65), [`theme-icon`](#link7468656d652d69636f6e).	 [→index](#idx) [→topic](#gui)

## `new-label` : procedure/1 {#link6e65772d6c6162656c}

Usage: `(new-label str) => int`

Creates a new text label with string `str.`

See also: [`set-label-text`](#link7365742d6c6162656c2d74657874).	 [→index](#idx) [→topic](#gui)

## `new-line` : procedure/1 or more {#link6e65772d6c696e65}

Usage: `(new-line fill-color [pos1] [pos2] [stroke-color] [stroke-width]) => int`

Draw and return a line with the given NRGBA `fill-color` from optional position `pos1` to position `pos2`, where these are lists of `x` and `y` coordinates as floats. The optional `stroke-color` and `stroke-width` determines the outer edges of the line.

See also: [`new-cirlce`](#link6e65772d6369726c6365), [`new-rectangle`](#link6e65772d72656374616e676c65), [`new-text`](#link6e65772d74657874).	 [→index](#idx) [→topic](#gui)

## `new-list` : procedure/3 {#link6e65772d6c697374}

Usage: `(new-list len-proc prep-proc update-proc) => int`

Create a new list display. A list consists of rows of simple items like labels. The `len-proc` must be a procedure without arguments returning the length of the list as integer. The `prep-proc` must be a procedure without arguments that returns a canvas object (i.e. a label or other widgets) representing a template for a single list item. The `update-proc` must be a procedure that receives the ID of a canvas object (given by the template) and the 0-based list index as arguments. This procedure then should modify the canvas object with `ID` to display the given list item at the index. See the GUI examples on how to use this function.

See also: [`new-table`](#link6e65772d7461626c65), [`new-tree`](#link6e65772d74726565).	 [→index](#idx) [→topic](#gui)

## `new-main-menu` : procedure/1 or more {#link6e65772d6d61696e2d6d656e75}

Usage: `(new-main-menu menu ...) => int`

Return a new main menu with the given menus. A main menu displays a menubar for a window on some desktop platforms but it may also be displayed in other ways.

See also: [`new-menu`](#link6e65772d6d656e75), [`new-menu*`](#link6e65772d6d656e752a).	 [→index](#idx) [→topic](#gui)

## `new-menu` : procedure/1 {#link6e65772d6d656e75}

Usage: `(new-menu menu*) => int`

Create a new visible menu widget from the abstract `menu*` created by new-menu*.

See also: [`new-menu*`](#link6e65772d6d656e752a), [`new-main-menu`](#link6e65772d6d61696e2d6d656e75).	 [→index](#idx) [→topic](#gui)

## `new-menu*` : procedure/1 or more {#link6e65772d6d656e752a}

Usage: `(new-menu* label [item...]) => int`

Make a new abstract menu with given `label` and arbitary menu items `item` ... following. The starred function is used to define a menu but is not bound to any particular way of displaying it (popup-menu, normal menu, main menu). Use `new-menu` and `new-main-menu` to create visible menus and menu bars based on such abstract menus.

See also: [`refresh-menu*`](#link726566726573682d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-main-menu`](#link6e65772d6d61696e2d6d656e75).	 [→index](#idx) [→topic](#gui)

## `new-menu-item` : procedure/2 or more {#link6e65772d6d656e752d6974656d}

Usage: `(new-menu-item str proc [selector...]) => int`

Create a new menu item with given label `str` and callback `proc`, which takes no arguments. The optional `selector` symbol may be one of: 'is-quit - the item is the application Quit menu item (this is dealt with differently by operating system requirements), 'is-separator - the item is a menu item separator and the label string is ignored (redundent, use `new-menu-item-separator` instead), 'disabled - the menu item is disabled, or 'checked - the menu item is checked.

See also: [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`menu-item-disabled?`](#link6d656e752d6974656d2d64697361626c65643f), [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx) [→topic](#gui)

## `new-menu-item-separator` : procedure/0 {#link6e65772d6d656e752d6974656d2d736570617261746f72}

Usage: `(new-menu-item-separator) => int`

Return a new menu item separator, which is a menu item without callback and label that displays a separator between menu items in menus.

See also: [`new-menu-item`](#link6e65772d6d656e752d6974656d).	 [→index](#idx) [→topic](#gui)

## `new-progress-bar` : procedure/0 {#link6e65772d70726f67726573732d626172}

Usage: `(new-progress-bar) => int`

Create a new progress bar whose default minimum is 0.0 and maximum is 1.0.

See also: [`set-progress-bar`](#link7365742d70726f67726573732d626172), [`new-infinite-progress-bar`](#link6e65772d696e66696e6974652d70726f67726573732d626172), [`get-progress-bar-value`](#link6765742d70726f67726573732d6261722d76616c7565).	 [→index](#idx) [→topic](#gui)

## `new-raster-with-pixels` : procedure/1 {#link6e65772d7261737465722d776974682d706978656c73}

Usage: `(new-raster-with-pixels pixel-proc) => int`

Create a new raster image generated dynamically by the given `pixel-proc`. The `pixel-proc` takes `x` and `y` pixel coordinates and the `width` and `height` of the image in pixels, and returns the color of the pixel `x`, `y` as a color list of the form '(red green blue [alpha]) where `alpha` is optional. Notice that specifying the color of each pixel can be very CPU-intensive for larger images, so optimizations might be necessary.

See also: [`new-image-from-file`](#link6e65772d696d6167652d66726f6d2d66696c65).	 [→index](#idx) [→topic](#gui)

## `new-rectangle` : procedure/1 or more {#link6e65772d72656374616e676c65}

Usage: `(new-rectangle fill-color [width height] [position] [stroke-color] [stroke-width] [corner-radius]) => int`

Draw and return a rectangle with the given NRGBA `fill-color`. The optional int `width` and `height` arguments set the width and height of the rectangle explicitly (otherwise they are 1). The optional `position` argument must be a list of `x` and `y` coordinates as floats. The optional `stroke-color` and `stroke-width` arguments determine the color and width of the outline of the rectangle, and the optional `corner-radious` defines how rounded the rectangle is. Notice that the rectangle's size and position can be set by the layout of the container, so to set it manually you need to make sure the underlying container has no layout that positions or resizes the rectangle.

See also: [`new-circle`](#link6e65772d636972636c65), [`new-line`](#link6e65772d6c696e65), [`new-text`](#link6e65772d74657874).	 [→index](#idx) [→topic](#gui)

## `new-regexp-validator` : procedure/2 {#link6e65772d7265676578702d76616c696461746f72}

Usage: `(new-regexp-validator regexp reason) => int`

Create a new string validator from the `regexp` string, which must be a valid regular expression in Go's regexp syntax. The `reason` string is displayed to the user by widgets like `entry` when the validation fails.

See also: [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72), [`new-validator`](#link6e65772d76616c696461746f72), [`new-combined-string-validator`](#link6e65772d636f6d62696e65642d737472696e672d76616c696461746f72), [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72), [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b), [`validate-object`](#link76616c69646174652d6f626a656374).	 [→index](#idx) [→topic](#gui)

## `new-scroll` : procedure/1 {#link6e65772d7363726f6c6c}

Usage: `(new-scroll obj) => int`

Embed canvas object `obj` into a new scroll container, which allows the user to scroll both horizontally and vertically if `obj` does not fit into the scroll container.

See also: [`new-vscroll`](#link6e65772d767363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c), [`new-container`](#link6e65772d636f6e7461696e6572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574).	 [→index](#idx) [→topic](#gui)

## `new-slider` : procedure/3 {#link6e65772d736c69646572}

Usage: `(new-slider min max proc) => int`

Create a new slider that allows users to adjust numerical values. The `min` and `max` arguments must be floats. The procedure `proc` takes the current slider float value as argument and is called when the slider changes.

See also: [`set-slider-value`](#link7365742d736c696465722d76616c7565).	 [→index](#idx) [→topic](#gui)

## `new-spacer` : procedure/0 {#link6e65772d737061636572}

Usage: `(new-spacer) => int`

Create a new spacer, which adjusts size dynamically by taking up space and displaying nothing. Use this to fill containers e.g. to right align a widget.

See also: [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx) [→topic](#gui)

## `new-stack-layout` : procedure/0 {#link6e65772d737461636b2d6c61796f7574}

Usage: `(new-stack-layout) => int`

Create a new stack layout that stacks container elements on top of each other, overlapping. This may be used for drawing, for example.

See also: [`new-form`](#link6e65772d666f726d), [`append-form`](#link617070656e642d666f726d), [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx) [→topic](#gui)

## `new-struct` : procedure/2 {#link6e65772d737472756374}

Usage: `(new-struct name li)`

Defines a new structure `name` with the properties in the a-list `li`. Structs are more leightweight than classes and do not allow for inheritance. Instances of structs ("records") are arrays.

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx) [→topic](#oop)

## `new-tabitem` : procedure/2 {#link6e65772d7461626974656d}

Usage: `(new-tabitem title obj) => int`

Create a new tab item for use in app-tabs and doc-tabs with a `title` and an embedded canvas object `obj` shown when the tab item is selected in the tabs.

See also: [`new-tabitem-with-icon`](#link6e65772d7461626974656d2d776974682d69636f6e), [`new-app-tabs`](#link6e65772d6170702d74616273), [`new-doc-tabs`](#link6e65772d646f632d74616273).	 [→index](#idx) [→topic](#gui)

## `new-tabitem-with-icon` : procedure/3 {#link6e65772d7461626974656d2d776974682d69636f6e}

Usage: `(new-tabitem-with-icon title icon obj) => int`

Create a new tab item for use in app-tabs and doc-tabs with given `title` string, `icon` resource, and embedded canvas object `obj` that shwon when the tab item is selected in the tabs.

See also: [`new-tabitem`](#link6e65772d7461626974656d), [`new-app-tabs`](#link6e65772d6170702d74616273), [`new-doc-tabs`](#link6e65772d646f632d74616273).	 [→index](#idx) [→topic](#gui)

## `new-table` : procedure/3 {#link6e65772d7461626c65}

Usage: `(new-table len-proc prep-proc update-proc) => int`

Create a new table display. A table consists of a number of rows, each of which has a fixed number of columns such as labels. The `len-proc` must be a procedure without arguments returning the length of the table as integer. The `prep-proc` must be a procedure without arguments that returns a canvas object that represents the table row with updatable columns. The `update-proc` takes the row, column, and ID of a canvas object and updates a table template with the right display for the table cell at `row` and `column.`

See also: [`new-list`](#link6e65772d6c697374), [`new-tree`](#link6e65772d74726565).	 [→index](#idx) [→topic](#gui)

## `new-text` : procedure/2 {#link6e65772d74657874}

Usage: `(new-text str color) => int`

Draw and return text with the given string `str` and foreground NRGBA `color.`

See also: [`set-text-alignment`](#link7365742d746578742d616c69676e6d656e74), [`set-text-size`](#link7365742d746578742d73697a65), [`set-text-style`](#link7365742d746578742d7374796c65), [`new-line`](#link6e65772d6c696e65), [`new-cirle`](#link6e65772d6369726c65), [`new-rectangle`](#link6e65772d72656374616e676c65).	 [→index](#idx) [→topic](#gui)

## `new-text-grid` : procedure/0 or more {#link6e65772d746578742d67726964}

Usage: `(new-text-grid [<string>] [show-line-numbers|show-whitespace|tab-width <int>]) => int`

Create a new text grid widget, which displays multiline text with custom background and foreground colors. The optional `string` argument is the initial text of the grid without formatting. The following symbols might be `'show-line-numbers` to turn the line number display on and `'show-whitespace` to display white space characters by special unicode symbols. If the selector `'tab-width` occurs, then it must be immediately followed by an integer for the tabulator width of the text grid in space characters.

See also: [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874), [`remove-text-grid-row`](#link72656d6f76652d746578742d677269642d726f77), [`insert-text-grid-row`](#link696e736572742d746578742d677269642d726f77).	 [→index](#idx) [→topic](#gui)

## `new-time-validator` : procedure/1 {#link6e65772d74696d652d76616c696461746f72}

Usage: `(new-time-validator format-str) => int`

Create a new string validator for time and date based on the given template `format-str`. This validator uses Go's data parsing function and therefore is quite restrictive. Only datetimes entered in exactly the format given (including timezones) validate successfully. To obtain a more relaxed date and time validator, use `new-validator` to create a custom validator with your own parsing or try a `new-regexp-validator.`

See also: [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72), [`new-validator`](#link6e65772d76616c696461746f72), [`new-combined-string-validator`](#link6e65772d636f6d62696e65642d737472696e672d76616c696461746f72), [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72), [`new-regexp-validator`](#link6e65772d7265676578702d76616c696461746f72), [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b), [`validate-object`](#link76616c69646174652d6f626a656374).	 [→index](#idx) [→topic](#gui)

## `new-tree` : procedure/4 {#link6e65772d74726565}

Usage: `(new-tree child-uid-proc is-branch-proc create-node-proc update-node-proc) => int`

Create a new tree display. A tree displays nested branches and leaf nodes. The `child-uid-proc` is a procedure that takes an id string as argument. If the string is empty, it should return a list of top-level branch uid strings. If the string is not empty, it represents an uid; the procedure should then return a list of all child id strings of that branch. This defines the tree's structure. All id strings must be unique to the tree. The `is-branch-proc` takes an id string as argument and should return non-nil if the id represents a branch, nil if it has no children. The `create-node-proc` takes a bool `b` as argument and should return a branch template if `b` is non-nil and a leaf template object if `b` is nil. Finally, the `update-node-proc` is a procedure that takes a node id string, a boolean that is true if the node is a branch, and a node template canvas-object as it is returned by `create-node-proc`. The procedure should fill the template with the display values for the respective node id.

See also: [`new-list`](#link6e65772d6c697374), [`new-table`](#link6e65772d7461626c65).	 [→index](#idx) [→topic](#gui)

## `new-validator` : procedure/1 {#link6e65772d76616c696461746f72}

Usage: `(new-validator proc) => int`

Create a new string validator based on validation procedure `proc`. The procedure `proc` takes a string as argument and returns a string. If the string returned is not the empty string "", then validation fails and the returned string is given as a reason for validation failure. If the empty string is returned, then validation succeeds. If an error occurs in `proc`, then validation fails with the error's error message as reason. Notice that validators are fairly limited and can only be attached to a few validatable objects such as text entry fields. For a more general approach, it might make sense to implement your own validation system based on key press, focus change, and change callbacks of various GUI objects.

See also: [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72), [`new-combined-string-validator`](#link6e65772d636f6d62696e65642d737472696e672d76616c696461746f72), [`new-regexp-validator`](#link6e65772d7265676578702d76616c696461746f72), [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72), [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b), [`validate-object`](#link76616c69646174652d6f626a656374).	 [→index](#idx) [→topic](#gui)

## `new-vbox-layout` : procedure/0 {#link6e65772d76626f782d6c61796f7574}

Usage: `(new-vbox-layout) => int`

Create a new vertical box layout, which lays out container elements vertically.

See also: [`new-spacer`](#link6e65772d737061636572), [`new-hbox-layout`](#link6e65772d68626f782d6c61796f7574), [`new-grid-layout`](#link6e65772d677269642d6c61796f7574), [`new-grid-wrap-layout`](#link6e65772d677269642d777261702d6c61796f7574), [`new-form-layout`](#link6e65772d666f726d2d6c61796f7574), [`new-center-layout`](#link6e65772d63656e7465722d6c61796f7574), [`new-stack-layout`](#link6e65772d737461636b2d6c61796f7574), [`new-container`](#link6e65772d636f6e7461696e6572).	 [→index](#idx) [→topic](#gui)

## `new-vscroll` : procedure/1 {#link6e65772d767363726f6c6c}

Usage: `(new-vscroll obj) => int`

Embed canvas object `obj` into a new vertical scroll container, which allows the user to scroll vertically if `obj` does not fit into the vscroll container vertically.

See also: [`new-scroll`](#link6e65772d7363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c), [`new-container`](#link6e65772d636f6e7461696e6572), [`new-vbox-layout`](#link6e65772d76626f782d6c61796f7574).	 [→index](#idx) [→topic](#gui)

## `new-vsplit` : procedure/2 {#link6e65772d7673706c6974}

Usage: `(new-vsplit obj1 obj2) => int`

Return a new vertical divider between canvas object `obj1` and `obj2`. The user can adjust the division by drag & drop.

See also: [`set-split-offset`](#link7365742d73706c69742d6f6666736574), [`new-hplit`](#link6e65772d68706c6974).	 [→index](#idx) [→topic](#gui)

## `new-window` : procedure/1 {#link6e65772d77696e646f77}

Usage: `(new-window title) => int`

Create a new window with `title` string and return the window ID. This function raises an error if the host configuration WindowsAllowed is not true. In certain embedded uses, creating new windows is not allowed and you should check the documentation how to find a pre-configured window and add user interface elements to it.

See also: [`set-window-content`](#link7365742d77696e646f772d636f6e74656e74), [`close-window`](#link636c6f73652d77696e646f77), [`show-window`](#link73686f772d77696e646f77).	 [→index](#idx) [→topic](#gui)

## `new-zedit` : procedure/3 {#link6e65772d7a65646974}

Usage: `(new-zedit columns lines canvas) => zedit`

Create a new zedit editor widget, which allows for source code editing and advanced syntax coloring but has other restrictions and does not support full text syles or embedded images.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`new-entry`](#link6e65772d656e747279).	 [→index](#idx) [→topic](#gui)

## `nl` : procedure/0 {#link6e6c}

Usage: `(nl)`

Display a newline, advancing the cursor to the next line.

See also: [`out`](#link6f7574), [`outy`](#link6f757479), [`output-at`](#link6f75747075742d6174).	 [→index](#idx) [→topic](#console)

## `nonce` : procedure/0 {#link6e6f6e6365}

Usage: `(nonce) => str`

Return a unique random string. This is not cryptographically secure but the string satisfies reasonable GUID requirements.

See also: [`externalize`](#link65787465726e616c697a65), [`internalize`](#link696e7465726e616c697a65).	 [→index](#idx) [→topic](#system)

## `not` : procedure/1 {#link6e6f74}

Usage: `(not x) => bool`

Return true if `x` is nil, nil otherwise.

See also: [`and`](#link616e64), [`or`](#link6f72).	 [→index](#idx) [→topic](#lisp)

## `now` : procedure/0 {#link6e6f77}

Usage: `(now) => li`

Return the current datetime in UTC format as a list of values in the form '((year month day weekday iso-week) (hour minute second nanosecond unix-nano-second)).

See also: [`now-ns`](#link6e6f772d6e73), [`datestr`](#link64617465737472), [`time`](#link74696d65), [`date->epoch-ns`](#link646174652d3e65706f63682d6e73), [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374).	 [→index](#idx) [→topic](#time)

## `now-ms` : procedure/0 {#link6e6f772d6d73}

Usage: `(now-ms) => num`

Return the relative system time as a call to (now-ns) but in milliseconds.

See also: [`now-ns`](#link6e6f772d6e73), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `now-ns` : procedure/0 {#link6e6f772d6e73}

Usage: `(now-ns) => int`

Return the current time in Unix nanoseconds.

See also: [`now`](#link6e6f77), [`time`](#link74696d65).	 [→index](#idx) [→topic](#time)

## `nreverse` : procedure/1 {#link6e72657665727365}

Usage: `(nreverse li) => li`

Destructively reverse `li.`

See also: [`reverse`](#link72657665727365).	 [→index](#idx) [→topic](#lisp)

## `nrgba` : procedure/4 {#link6e72676261}

Usage: `(nrgba red green blue alpha) => int`

Create an RGBA color where `red`, `green`, `blue`, and `alpha` are 8-bit uint integers, i.e., values between 0 and 255 (inclusive). Notice that some GUI functions require NRGBA color returned by this function, whereas others require a color list of int values '(red green blue alpha). This is for performance reasons, since it sometimes faster to convert a list to a color on-the-fly and sometimes more convenient to store pre-defined colors for later re-use.

See also: [`nrgba64`](#link6e726762613634), [`theme-color`](#link7468656d652d636f6c6f72), [`new-rectangle`](#link6e65772d72656374616e676c65), [`new-circle`](#link6e65772d636972636c65), [`new-line`](#link6e65772d6c696e65), [`new-text`](#link6e65772d74657874).	 [→index](#idx) [→topic](#gui)

## `nrgba64` : procedure/4 {#link6e726762613634}

Usage: `(nrgba64 red green blue alpha) => int`

Create a 64-bit RGBA color where `red`, `green`, `blue`, and `alpha` are 16-bit uint integers, i.e., values between 0 and 65365 (inclusive). Notice that some GUI functions require NRGBA64 color returned by this function, whereas others require a color list of int values '(red green blue alpha). This is for performance reasons, since it sometimes faster to convert a list to a color on-the-fly and sometimes more convenient to store pre-defined colors for later re-use.

See also: [`nrgba`](#link6e72676261), [`theme-color`](#link7468656d652d636f6c6f72), [`new-rectangle`](#link6e65772d72656374616e676c65), [`new-circle`](#link6e65772d636972636c65), [`new-line`](#link6e65772d6c696e65), [`new-text`](#link6e65772d74657874).	 [→index](#idx) [→topic](#gui)

## `nth` : procedure/2 {#link6e7468}

Usage: `(nth seq n) => any`

Get the `n-th` element of sequence `seq`. Sequences are 0-indexed.

See also: [`nthdef`](#link6e7468646566), [`list`](#link6c697374), [`array`](#link6172726179), [`string`](#link737472696e67), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `nth-partition` : procedure/3 {#link6e74682d706172746974696f6e}

Usage: `(nth-partition m k idx) => li`

Return a list of the form (start-offset end-offset bytes) for the partition with index `idx` of `m` into parts of size `k`. The index `idx` as well as the start- and end-offsets are 0-based.

See also: [`count-partitions`](#link636f756e742d706172746974696f6e73), [`get-partitions`](#link6765742d706172746974696f6e73).	 [→index](#idx) [→topic](#lisp)

## `nthdef` : procedure/3 {#link6e7468646566}

Usage: `(nthdef seq n default) => any`

Return the `n-th` element of sequence `seq` (0-indexed) if `seq` is a sequence and has at least `n+1` elements, default otherwise.

See also: [`nth`](#link6e7468), [`seq?`](#link7365713f), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`5th`](#link357468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468).	 [→index](#idx) [→topic](#seq)

## `null?` : procedure/1 {#link6e756c6c3f}

Usage: `(null? li) => bool`

Return true if `li` is nil, nil otherwise.

See also: [`not`](#link6e6f74), [`list?`](#link6c6973743f), [`cons?`](#link636f6e733f).	 [→index](#idx) [→topic](#lisp)

## `num?` : procedure/1 {#link6e756d3f}

Usage: `(num? n) => bool`

Return true if `n` is a number (exact or inexact), nil otherwise.

See also: [`str?`](#link7374723f), [`atom?`](#link61746f6d3f), [`sym?`](#link73796d3f), [`closure?`](#link636c6f737572653f), [`intrinsic?`](#link696e7472696e7369633f), [`macro?`](#link6d6163726f3f).	 [→index](#idx) [→topic](#lisp)

## `object-disabled?` : procedure/1 {#link6f626a6563742d64697361626c65643f}

Usage: `(object-disabled? obj) => bool`

Return true if the canvas object `obj` is disabled, nil otherwise.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `object?` : procedure/1 {#link6f626a6563743f}

Usage: `(object? obj) => bool`

Return true of `obj` is an object array, nil otherwise.

See also: [`class?`](#link636c6173733f), [`isa?`](#link6973613f).	 [→index](#idx) [→topic](#oop)

## `odd?` : procedure/1 {#link6f64643f}

Usage: `(odd? n) => bool`

Returns true if the integer `n` is odd, nil otherwise.

See also: [`even?`](#link6576656e3f).	 [→index](#idx) [→topic](#numeric)

## `on-feature` : macro/1 or more {#link6f6e2d66656174757265}

Usage: `(on-feature sym body ...) => any`

Evaluate the expressions of `body` if the Lisp feature `sym` is supported by this implementation, do nothing otherwise.

See also: [`feature?`](#link666561747572653f), [`*reflect*`](#link2a7265666c6563742a).	 [→index](#idx) [→topic](#system)

## `open` : procedure/1 or more {#link6f70656e}

Usage: `(open file-path [modes] [permissions]) => int`

Open the file at `file-path` for reading and writing, and return the stream ID. The optional `modes` argument must be a list containing one of '(read write read-write) for read, write, or read-write access respectively, and may contain any of the following symbols: 'append to append to an existing file, 'create for creating the file if it doesn't exist, 'exclusive for exclusive file access, 'truncate for truncating the file if it exists, and 'sync for attempting to sync file access. The optional `permissions` argument must be a numeric value specifying the Unix file permissions of the file. If these are omitted, then default values '(read-write append create) and 0640 are used.

See also: [`stropen`](#link7374726f70656e), [`close`](#link636c6f7365), [`read`](#link72656164), [`write`](#link7772697465).	 [→index](#idx) [→topic](#fileio)

## `or` : macro/0 or more {#link6f72}

Usage: `(or expr1 expr2 ...) => any`

Evaluate the expressions until one of them is not nil. This is a logical shortcut or.

See also: [`and`](#link616e64).	 [→index](#idx) [→topic](#lisp)

## `out` : procedure/1 {#link6f7574}

Usage: `(out expr)`

Output `expr` on the console with current default background and foreground color.

See also: [`outy`](#link6f757479), [`synout`](#link73796e6f7574), [`synouty`](#link73796e6f757479), [`output-at`](#link6f75747075742d6174).	 [→index](#idx) [→topic](#ui)

## `outy` : procedure/1 {#link6f757479}

Usage: `(outy spec)`

Output styled text specified in `spec`. A specification is a list of lists starting with 'fg for foreground, 'bg for background, or 'text for unstyled text. If the list starts with 'fg or 'bg then the next element must be a color suitable for (the-color spec). Following may be a string to print or another color specification. If a list starts with 'text then one or more strings may follow.

See also: [`*colors*`](#link2a636f6c6f72732a), [`the-color`](#link7468652d636f6c6f72), [`set-color`](#link7365742d636f6c6f72), [`color`](#link636f6c6f72), [`gfx.color`](#link6766782e636f6c6f72), [`output-at`](#link6f75747075742d6174), [`out`](#link6f7574).	 [→index](#idx) [→topic](#ui)

## `peek` : procedure/4 {#link7065656b}

Usage: `(peek b pos end sel) => num`

Read a numeric value determined by selector `sel` from binary blob `b` at position `pos` with endianness `end`. Possible values for endianness are 'little and 'big, and possible values for `sel` must be one of '(bool int8 uint8 int16 uint16 int32 uint32 int64 uint64 float32 float64).

See also: [`poke`](#link706f6b65), [`read-binary`](#link726561642d62696e617279).	 [→index](#idx) [→topic](#binary)

## `permission?` : procedure/1 {#link7065726d697373696f6e3f}

Usage: `(permission? sym [default]) => bool`

Return true if the permission for `sym` is set, nil otherwise. If the permission flag is unknown, then `default` is returned. The default for `default` is nil.

See also: [`permissions`](#link7065726d697373696f6e73), [`set-permissions`](#link7365742d7065726d697373696f6e73), [`when-permission`](#link7768656e2d7065726d697373696f6e), [`sys`](#link737973).	 [→index](#idx) [→topic](#system)

## `permissions` : procedure/0 {#link7065726d697373696f6e73}

Usage: `(permissions)`

Return a list of all active permissions of the current interpreter. Permissions are: `load-prelude` - load the init file on start; `load-user-init` - load the local user init on startup, file if present; `allow-unprotect` - allow the user to unprotect protected symbols (for redefining them); `allow-protect` - allow the user to protect symbols from redefinition or unbinding; `interactive` - make the session interactive, this is particularly used during startup to determine whether hooks are installed and feedback is given. Permissions have to generally be set or removed in careful combination with `revoke-permissions`, which redefines symbols and functions.

See also: [`set-permissions`](#link7365742d7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f), [`when-permission`](#link7768656e2d7065726d697373696f6e), [`sys`](#link737973).	 [→index](#idx) [→topic](#system)

## `poke` : procedure/5 {#link706f6b65}

Usage: `(poke b pos end sel n)`

Write numeric value `n` as type `sel` with endianness `end` into the binary blob `b` at position `pos`. Possible values for endianness are 'little and 'big, and possible values for `sel` must be one of '(bool int8 uint8 int16 uint16 int32 uint32 int64 uint64 float32 float64).

See also: [`peek`](#link7065656b), [`write-binary`](#link77726974652d62696e617279).	 [→index](#idx) [→topic](#binary)

## `pop!` : macro/1 or more {#link706f7021}

Usage: `(pop! sym [def]) => any`

Get the next element from stack `sym`, which must be the unquoted name of a variable, and return it. If a default `def` is given, then this is returned if the queue is empty, otherwise nil is returned.

See also: [`make-stack`](#link6d616b652d737461636b), [`stack?`](#link737461636b3f), [`push!`](#link7075736821), [`stack-len`](#link737461636b2d6c656e), [`stack-empty?`](#link737461636b2d656d7074793f), [`glance`](#link676c616e6365).	 [→index](#idx) [→topic](#data)

## `pop-error-handler` : procedure/0 {#link706f702d6572726f722d68616e646c6572}

Usage: `(pop-error-handler) => proc`

Remove the topmost error handler from the error handler stack and return it. For internal use only.

See also: [`with-error-handler`](#link776974682d6572726f722d68616e646c6572).	 [→index](#idx) [→topic](#system)

## `pop-finalizer` : procedure/0 {#link706f702d66696e616c697a6572}

Usage: `(pop-finalizer) => proc`

Remove a finalizer from the finalizer stack and return it. For internal use only.

See also: [`push-finalizer`](#link707573682d66696e616c697a6572), [`with-final`](#link776974682d66696e616c).	 [→index](#idx) [→topic](#system)

## `popstacked` : procedure/3 {#link706f70737461636b6564}

Usage: `(popstacked dict key default)`

Get the topmost element from the stack stored at `key` in `dict` and remove it from the stack. If the stack is empty or no stack is stored at key, then `default` is returned.

See also: [`pushstacked`](#link70757368737461636b6564), [`getstacked`](#link676574737461636b6564).	 [→index](#idx) [→topic](#dict)

## `prin1` : procedure/1 {#link7072696e31}

Usage: `(prin1 s)`

Print `s` to the host OS terminal, where strings are quoted.

See also: [`princ`](#link7072696e63), [`terpri`](#link746572707269), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx) [→topic](#console)

## `princ` : procedure/1 {#link7072696e63}

Usage: `(princ s)`

Print `s` to the host OS terminal without quoting strings.

See also: [`prin1`](#link7072696e31), [`terpri`](#link746572707269), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx) [→topic](#console)

## `print` : procedure/1 {#link7072696e74}

Usage: `(print x)`

Output `x` on the host OS console and end it with a newline.

See also: [`prin1`](#link7072696e31), [`princ`](#link7072696e63).	 [→index](#idx) [→topic](#console)

## `print-zedit` : procedure/3 {#link7072696e742d7a65646974}

Usage: `(print-zedit zedit s tags)`

Print text `s` at the current caret position and apply `tags` to the printed text.

See also: [`new-zedit`](#link6e65772d7a65646974), [`insert-zedit`](#link696e736572742d7a65646974), [`delete-zedit`](#link64656c6574652d7a65646974).	 [→index](#idx) [→topic](#gui)

## `proc?` : macro/1 {#link70726f633f}

Usage: `(proc? arg) => bool`

Return true if `arg` is a procedure, nil otherwise.

See also: [`functional?`](#link66756e6374696f6e616c3f), [`closure?`](#link636c6f737572653f), [`functional-arity`](#link66756e6374696f6e616c2d6172697479), [`functional-has-rest?`](#link66756e6374696f6e616c2d6861732d726573743f).	 [→index](#idx) [→topic](#system)

## `prop` : procedure/2 {#link70726f70}

Usage: `(prop obj slot) => any`

Return the value in `obj` for property `slot`, or an error if the object does not have a property with that name.

See also: [`new`](#link6e6577), [`isa?`](#link6973613f), [`setprop`](#link73657470726f70), [`object?`](#link6f626a6563743f), [`class-name`](#link636c6173732d6e616d65), [`supers`](#link737570657273), [`props`](#link70726f7073), [`methods`](#link6d6574686f6473), [`has-prop?`](#link6861732d70726f703f).	 [→index](#idx) [→topic](#oop)

## `props` : procedure/1 {#link70726f7073}

Usage: `(props obj) => li`

Return the list of properties of `obj`. An error occurs if `obj` is not a valid object.

See also: [`methods`](#link6d6574686f6473), [`has-prop?`](#link6861732d70726f703f), [`new`](#link6e6577), [`prop`](#link70726f70), [`setprop`](#link73657470726f70).	 [→index](#idx) [→topic](#oop)

## `protect` : procedure/0 or more {#link70726f74656374}

Usage: `(protect [sym] ...)`

Protect symbols `sym` ... against changes or rebinding. The symbols need to be quoted. This operation requires the permission 'allow-protect to be set.

See also: [`protected?`](#link70726f7465637465643f), [`unprotect`](#link756e70726f74656374), [`dict-protect`](#link646963742d70726f74656374), [`dict-unprotect`](#link646963742d756e70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f), [`setq`](#link73657471), [`bind`](#link62696e64), [`interpret`](#link696e74657270726574).	 [→index](#idx) [→topic](#system)

## `protect-toplevel-symbols` : procedure/0 {#link70726f746563742d746f706c6576656c2d73796d626f6c73}

Usage: `(protect-toplevel-symbols)`

Protect all toplevel symbols that are not yet protected and aren't in the *mutable-toplevel-symbols* dict.

See also: [`protected?`](#link70726f7465637465643f), [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`declare-unprotected`](#link6465636c6172652d756e70726f746563746564), [`declare-volatile`](#link6465636c6172652d766f6c6174696c65), [`when-permission?`](#link7768656e2d7065726d697373696f6e3f), [`dict-protect`](#link646963742d70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`dict-unprotect`](#link646963742d756e70726f74656374).	 [→index](#idx) [→topic](#system)

## `protected?` : procedure/1 {#link70726f7465637465643f}

Usage: `(protected? sym)`

Return true if `sym` is protected, nil otherwise.

See also: [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`dict-unprotect`](#link646963742d756e70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`permission`](#link7065726d697373696f6e), [`permission?`](#link7065726d697373696f6e3f), [`setq`](#link73657471), [`bind`](#link62696e64), [`interpret`](#link696e74657270726574).	 [→index](#idx) [→topic](#system)

## `prune-task-table` : procedure/0 {#link7072756e652d7461736b2d7461626c65}

Usage: `(prune-task-table)`

Remove tasks that are finished from the task table. This includes tasks for which an error has occurred.

See also: [`task-remove`](#link7461736b2d72656d6f7665), [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e).	 [→index](#idx) [→topic](#concurrency)

## `prune-unneeded-help-entries` : procedure/0 {#link7072756e652d756e6e65656465642d68656c702d656e7472696573}

Usage: `(prune-unneeded-help-entries)`

Remove help entries for which no toplevel symbol is defined. This function may need to be called when a module is not being used (e.g. because of a missing build tag) and it is desirable that only help for existing symbols is available.

See also: [`find-unneeded-help-entries`](#link66696e642d756e6e65656465642d68656c702d656e7472696573), [`find-missing-help-entries`](#link66696e642d6d697373696e672d68656c702d656e7472696573), [`help`](#link68656c70), [`*help*`](#link2a68656c702a).	 [→index](#idx) [→topic](#help)

## `push!` : macro/2 {#link7075736821}

Usage: `(push! sym elem)`

Put `elem` in stack `sym`, where `sym` is the unquoted name of a variable.

See also: [`make-stack`](#link6d616b652d737461636b), [`stack?`](#link737461636b3f), [`pop!`](#link706f7021), [`stack-len`](#link737461636b2d6c656e), [`stack-empty?`](#link737461636b2d656d7074793f), [`glance`](#link676c616e6365).	 [→index](#idx) [→topic](#data)

## `push-error-handler` : procedure/1 {#link707573682d6572726f722d68616e646c6572}

Usage: `(push-error-handler proc)`

Push an error handler `proc` on the error handler stack. For internal use only.

See also: [`with-error-handler`](#link776974682d6572726f722d68616e646c6572).	 [→index](#idx) [→topic](#system)

## `push-finalizer` : procedure/1 {#link707573682d66696e616c697a6572}

Usage: `(push-finalizer proc)`

Push a finalizer procedure `proc` on the finalizer stack. For internal use only.

See also: [`with-final`](#link776974682d66696e616c), [`pop-finalizer`](#link706f702d66696e616c697a6572).	 [→index](#idx) [→topic](#system)

## `pushstacked` : procedure/3 {#link70757368737461636b6564}

Usage: `(pushstacked dict key datum)`

Push `datum` onto the stack maintained under `key` in the `dict.`

See also: [`getstacked`](#link676574737461636b6564), [`popstacked`](#link706f70737461636b6564).	 [→index](#idx) [→topic](#dict)

## `queue-empty?` : procedure/1 {#link71756575652d656d7074793f}

Usage: `(queue-empty? q) => bool`

Return true if the queue `q` is empty, nil otherwise.

See also: [`make-queue`](#link6d616b652d7175657565), [`queue?`](#link71756575653f), [`enqueue!`](#link656e717565756521), [`dequeue!`](#link6465717565756521), [`glance`](#link676c616e6365), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx) [→topic](#data)

## `queue-len` : procedure/1 {#link71756575652d6c656e}

Usage: `(queue-len q) => int`

Return the length of the queue `q.`

See also: [`make-queue`](#link6d616b652d7175657565), [`queue?`](#link71756575653f), [`enqueue!`](#link656e717565756521), [`dequeue!`](#link6465717565756521), [`glance`](#link676c616e6365), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx)

**Warning: Be advised that this is of limited use in some concurrent contexts, since the length of the queue might have changed already once you've obtained it!** [→topic](#data)

## `queue?` : procedure/1 {#link71756575653f}

Usage: `(queue? q) => bool`

Return true if `q` is a queue, nil otherwise.

See also: [`make-queue`](#link6d616b652d7175657565), [`enqueue!`](#link656e717565756521), [`dequeue`](#link64657175657565), [`glance`](#link676c616e6365), [`queue-empty?`](#link71756575652d656d7074793f), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx) [→topic](#data)

## `rand` : procedure/2 {#link72616e64}

Usage: `(rand prng lower upper) => int`

Return a random integer in the interval [`lower`` upper`], both inclusive, from pseudo-random number generator `prng`. The `prng` argument must be an integer from 0 to 9 (inclusive).

See also: [`rnd`](#link726e64), [`rndseed`](#link726e6473656564).	 [→index](#idx) [→topic](#numeric)

## `random-color` : procedure/0 or more {#link72616e646f6d2d636f6c6f72}

Usage: `(random-color [alpha])`

Return a random color with optional `alpha` value. If `alpha` is not specified, it is 255.

See also: [`the-color`](#link7468652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a), [`darken`](#link6461726b656e), [`lighten`](#link6c69676874656e).	 [→index](#idx) [→topic](#ui)

## `read` : procedure/1 {#link72656164}

Usage: `(read p) => any`

Read an expression from input port `p.`

See also: [`input`](#link696e707574), [`write`](#link7772697465).	 [→index](#idx) [→topic](#fileio)

## `read-binary` : procedure/3 {#link726561642d62696e617279}

Usage: `(read-binary p buff n) => int`

Read `n` or less bytes from input port `p` into binary blob `buff`. If `buff` is smaller than `n`, then an error is raised. If less than `n` bytes are available before the end of file is reached, then the amount k of bytes is read into `buff` and k is returned. If the end of file is reached and no byte has been read, then 0 is returned. So to loop through this, read into the buffer and do something with it while the amount of bytes returned is larger than 0.

See also: [`write-binary`](#link77726974652d62696e617279), [`read`](#link72656164), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx) [→topic](#fileio)

## `read-string` : procedure/2 {#link726561642d737472696e67}

Usage: `(read-string p delstr) => str`

Reads a string from port `p` until the single-byte delimiter character in `delstr` is encountered, and returns the string including the delimiter. If the input ends before the delimiter is encountered, it returns the string up until EOF. Notice that if the empty string is returned then the end of file must have been encountered, since otherwise the string would contain the delimiter.

See also: [`read`](#link72656164), [`read-binary`](#link726561642d62696e617279), [`write-string`](#link77726974652d737472696e67), [`write`](#link7772697465), [`read`](#link72656164), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx) [→topic](#fileio)

## `read-zimage` : procedure/2 {#link726561642d7a696d616765}

Usage: `(read-zimage in fi)`

Reads and evaluates the zimage in stream `in` from file `fi`. The file `fi` argument is used in error messages. This procedure raises errors when the zimage is malformed or the version check fails.

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`run-zimage`](#link72756e2d7a696d616765), [`zimage-header`](#link7a696d6167652d686561646572).	 [→index](#idx) [→topic](#zimage)

## `readall` : procedure/1 {#link72656164616c6c}

Usage: `(readall stream) => sexpr`

Read all data from `stream` and return it as an sexpr.

See also: [`read`](#link72656164), [`write`](#link7772697465), [`open`](#link6f70656e), [`close`](#link636c6f7365).	 [→index](#idx)

## `readall-str` : procedure/1 or more {#link72656164616c6c2d737472}

Usage: `(readall-str p [buffsize]) => str`

Read all content from port `p` as string. This method may trigger an error if the content in the stream is not a valid UTF-8 string. The optional `buffsize` argument determines the size of the internal buffer.

See also: [`readall`](#link72656164616c6c), [`read-binary`](#link726561642d62696e617279), [`read`](#link72656164).	 [→index](#idx)

## `recall` : procedure/1 or more {#link726563616c6c}

Usage: `(recall key [notfound]) => any`

Obtain the value remembered for `key`, `notfound` if it doesn't exist. If `notfound` is not provided, then nil is returned in case the value for `key` doesn't exist.

See also: [`recall-when`](#link726563616c6c2d7768656e), [`recall-info`](#link726563616c6c2d696e666f), [`recollect`](#link7265636f6c6c656374), [`remember`](#link72656d656d626572), [`forget`](#link666f72676574).	 [→index](#idx) [→topic](#db)

## `recall-info` : procedure/1 or more {#link726563616c6c2d696e666f}

Usage: `(recall-info key [notfound]) => (str str)`

Return a list containing the info string and its fuzzy version for a remembered value with the given `key`, `notfound` if no value for `key` was found. The default for `notfound` is nil.

See also: [`recall-when`](#link726563616c6c2d7768656e), [`recall`](#link726563616c6c), [`recall-when`](#link726563616c6c2d7768656e), [`recollect`](#link7265636f6c6c656374), [`remember`](#link72656d656d626572), [`forget`](#link666f72676574).	 [→index](#idx) [→topic](#db)

## `recall-when` : procedure/1 or more {#link726563616c6c2d7768656e}

Usage: `(recall-when key [notfound]) => datestr`

Obtain the date string when the value for `key` was last modified by remember (set), `notfound` if it doesn't exist. If `notfound` is not provided, then nil is returned in case there is no value for `key.`

See also: [`recall`](#link726563616c6c), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`recall-info`](#link726563616c6c2d696e666f), [`remember`](#link72656d656d626572), [`forget`](#link666f72676574).	 [→index](#idx) [→topic](#db)

## `recollect` : procedure/1 or more {#link7265636f6c6c656374}

Usage: `(recollect s [keytype] [limit] [fuzzer]) => li`

Search for remembered items based on search query `s` and return a list of matching keys. The optional `keytype` parameter must be one of '(all str sym int expr), where the default is 'all for all kinds of keys. Up to `limit` results are returned, default is kvdb.*default-search-limit*. The optional `fuzzer` procedure takes a word string and yields a 'fuzzy' version of it. If fuzzer is specified and a procedure, then a fuzzy search is performed.

See also: [`kvdb.search`](#link6b7664622e736561726368), [`recall`](#link726563616c6c), [`recall-info`](#link726563616c6c2d696e666f), [`recall-when`](#link726563616c6c2d7768656e), [`remember`](#link72656d656d626572).	 [→index](#idx) [→topic](#db)

## `record?` : procedure/1 {#link7265636f72643f}

Usage: `(record? s) => bool`

Returns true if `s` is a struct record, i.e., an instance of a struct; nil otherwise. Notice that records are not really types distinct from arrays, they simply contain a marker '%record as first element. With normal use no confusion should arise. Since the internal representation might change, you ought not use ordinary array procedures for records.

See also: [`struct?`](#link7374727563743f), [`defstruct`](#link646566737472756374).	 [→index](#idx) [→topic](#oop)

## `refresh-main-menu` : procedure/1 {#link726566726573682d6d61696e2d6d656e75}

Usage: `(refresh-main-menu main-menu)`

Refresh the given `main-menu` display. This should be called after some submenus or menu items in the main menu have changed.

See also: [`new-main-menu`](#link6e65772d6d61696e2d6d656e75), [`refresh-menu*`](#link726566726573682d6d656e752a).	 [→index](#idx) [→topic](#gui)

## `refresh-menu*` : procedure/1 {#link726566726573682d6d656e752a}

Usage: `(refresh-menu* menu)`

Refresh the given `menu` after a change was made that has a visual impact. This will refresh the menu widget in which this abstract menu occurs.

See also: [`refresh-main-menu`](#link726566726573682d6d61696e2d6d656e75), [`new-menu*`](#link6e65772d6d656e752a).	 [→index](#idx) [→topic](#gui)

## `refresh-object` : procedure/1 {#link726566726573682d6f626a656374}

Usage: `(refresh-object obj)`

Refresh the canvas object `obj`, causing the graphical display to be re-drawn as soon as possible. This may be needed if the object's state has changed.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `register-action` : procedure/1 {#link72656769737465722d616374696f6e}

Usage: `(register-action action)`

Register the `action` which makes it available for processing by the host system. Use `get-action` to obtain an action clone that can be started.

See also: [`action`](#link616374696f6e), [`has-action-system?`](#link6861732d616374696f6e2d73797374656d3f), [`action-start`](#link616374696f6e2d7374617274), [`action-stop`](#link616374696f6e2d73746f70).	 [→index](#idx) [→topic](#action)

## `remember` : procedure/2 {#link72656d656d626572}

Usage: `(remember key value [info] [fuzzer])`

Persistently remember `value` by given `key`. See kvdb.set for the optional `info` and `fuzzer` arguments.

See also: [`recall`](#link726563616c6c), [`forget`](#link666f72676574), [`kvdb.set`](#link6b7664622e736574), [`recall-when`](#link726563616c6c2d7768656e), [`recall-info`](#link726563616c6c2d696e666f), [`recollect`](#link7265636f6c6c656374).	 [→index](#idx) [→topic](#db)

## `remove-canvas-shortcut` : procedure/2 {#link72656d6f76652d63616e7661732d73686f7274637574}

Usage: `(remove-canvas-shortcut canvas shortcut)`

Remove the `shortcut` from `canvas`, where `shortcut` is a list consisting of valid keyboard modifier symbols and a valid key symbol.

See also: [`add-canvas-shortcut`](#link6164642d63616e7661732d73686f7274637574), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173).	 [→index](#idx) [→topic](#gui)

## `remove-duplicates` : procedure/1 {#link72656d6f76652d6475706c696361746573}

Usage: `(remove-duplicates seq) => seq`

Remove all duplicates in sequence `seq`, return a new sequence with the duplicates removed.

See also: [`seq?`](#link7365713f), [`map`](#link6d6170), [`foreach`](#link666f7265616368), [`nth`](#link6e7468).	 [→index](#idx) [→topic](#seq)

## `remove-hook` : procedure/2 {#link72656d6f76652d686f6f6b}

Usage: `(remove-hook hook id) => bool`

Remove the symbolic or numberic `hook` with `id` and return true if the hook was removed, nil otherwise.

See also: [`add-hook`](#link6164642d686f6f6b), [`remove-hooks`](#link72656d6f76652d686f6f6b73), [`replace-hook`](#link7265706c6163652d686f6f6b).	 [→index](#idx) [→topic](#system)

## `remove-hook-internal` : procedure/2 {#link72656d6f76652d686f6f6b2d696e7465726e616c}

Usage: `(remove-hook-internal hook id)`

Remove the hook with ID `id` from numeric `hook.`

See also: [`remove-hook`](#link72656d6f76652d686f6f6b).	 [→index](#idx)

**Warning: Internal use only.** [→topic](#system)

## `remove-hooks` : procedure/1 {#link72656d6f76652d686f6f6b73}

Usage: `(remove-hooks hook) => bool`

Remove all hooks for symbolic or numeric `hook`, return true if the hook exists and the associated procedures were removed, nil otherwise.

See also: [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b), [`replace-hook`](#link7265706c6163652d686f6f6b).	 [→index](#idx) [→topic](#system)

## `remove-text-grid-row` : procedure/2 {#link72656d6f76652d746578742d677269642d726f77}

Usage: `(remove-text-grid-row grid row)`

Remove the `row` from the given text `grid`. An incorrect `row` index will result in an error.

See also: [`insert-text-grid-row`](#link696e736572742d746578742d677269642d726f77), [`new-text-grid`](#link6e65772d746578742d67726964), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77).	 [→index](#idx) [→topic](#gui)

## remove-zedit-event-handler : nil {#link72656d6f76652d7a656469742d6576656e742d68616e646c6572}

Usage: `(remove-zedit-event-handler editor sel)`

Remove the event handler for `sel` in `editor`, do nothing if no handler has been set for the event. The selector `sel` must be one of '(caret-move).

See also: [`new-zedit`](#link6e65772d7a65646974), [`set-zedit-event-handler`](#link7365742d7a656469742d6576656e742d68616e646c6572).	 [→index](#idx) [→topic](#gui)

## `rename-action` : procedure/2 {#link72656e616d652d616374696f6e}

Usage: `(rename-action id new-name) => bool`

Rename a registered action with given `id`, or rename the action given as `id`, to `new-name`. If the operation succeeds, it returns true, otherwise it returns nil.

See also: [`change-action-prefix`](#link6368616e67652d616374696f6e2d707265666978), [`change-all-action-prefixes`](#link6368616e67652d616c6c2d616374696f6e2d7072656669786573), [`get-action`](#link6765742d616374696f6e), [`has-action?`](#link6861732d616374696f6e3f), [`action`](#link616374696f6e).	 [→index](#idx) [→topic](#action)

## `replace-hook` : procedure/2 {#link7265706c6163652d686f6f6b}

Usage: `(replace-hook hook proc)`

Remove all hooks for symbolic or numeric `hook` and install the given `proc` as the only hook procedure.

See also: [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b), [`remove-hooks`](#link72656d6f76652d686f6f6b73).	 [→index](#idx) [→topic](#system)

## `reset-color` : procedure/0 {#link72657365742d636f6c6f72}

Usage: `(reset-color)`

Reset the 'text and 'back colors of the display to default values. These values are not specified in the color database and depend on the runtime implementation. Other colors like 'gfx or 'frame are not affected.

See also: [`set-color`](#link7365742d636f6c6f72), [`color`](#link636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`with-colors`](#link776974682d636f6c6f7273).	 [→index](#idx) [→topic](#ui)

## `resize-object` : procedure/2 {#link726573697a652d6f626a656374}

Usage: `(resize-object obj width height)`

Resize canvas object `obj` to the given `width` and `height` as floats.

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`show-object`](#link73686f772d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `reverse` : procedure/1 {#link72657665727365}

Usage: `(reverse seq) => sequence`

Reverse a sequence non-destructively, i.e., return a copy of the reversed sequence.

See also: [`nth`](#link6e7468), [`seq?`](#link7365713f), [`1st`](#link317374), [`2nd`](#link326e64), [`3rd`](#link337264), [`4th`](#link347468), [`6th`](#link367468), [`7th`](#link377468), [`8th`](#link387468), [`9th`](#link397468), [`10th`](#link31307468), [`last`](#link6c617374).	 [→index](#idx) [→topic](#seq)

## `rnd` : procedure/0 {#link726e64}

Usage: `(rnd prng) => num`

Return a random value in the interval [0, 1] from pseudo-random number generator `prng`. The `prng` argument must be an integer from 0 to 9 (inclusive).

See also: [`rand`](#link72616e64), [`rndseed`](#link726e6473656564).	 [→index](#idx) [→topic](#numeric)

## `rndseed` : procedure/1 {#link726e6473656564}

Usage: `(rndseed prng n)`

Seed the pseudo-random number generator `prng` (0 to 9) with 64 bit integer value `n`. Larger values will be truncated. Seeding affects both the rnd and the rand function for the given `prng.`

See also: [`rnd`](#link726e64), [`rand`](#link72616e64).	 [→index](#idx) [→topic](#numeric)

## `rplaca` : procedure/2 {#link72706c616361}

Usage: `(rplaca li a) => li`

Destructively mutate `li` such that its car is `a`, return the list afterwards.

See also: [`rplacd`](#link72706c616364).	 [→index](#idx) [→topic](#lisp)

## `run-at` : procedure/2 {#link72756e2d6174}

Usage: `(run-at date repeater proc) => int`

Run procedure `proc` with no arguments as task periodically according to the specification in `spec` and return the task ID for the periodic task. Herbey, `date` is either a datetime specification or one of '(now skip next-minute next-quarter next-halfhour next-hour in-2-hours in-3-hours tomorrow next-week next-month next-year), and `repeater` is nil or a procedure that takes a task ID and unix-epoch-nanoseconds and yields a new unix-epoch-nanoseconds value for the next time the procedure shall be run. While the other names are self-explanatory, the 'skip specification means that the task is not run immediately but rather that it is first run at (repeater -1 (now)). Timing resolution for the scheduler is about 1 minute. Consider using interrupts for periodic events with smaller time resolutions. The scheduler uses relative intervals and has 'drift'.

See also: [`task`](#link7461736b), [`task-send`](#link7461736b2d73656e64).	 [→index](#idx)

**Warning: Tasks scheduled by run-at are not persistent! They are only run until the system is shutdown.** [→topic](#concurrency)

## `run-hook` : procedure/1 {#link72756e2d686f6f6b}

Usage: `(run-hook hook)`

Manually run the hook, executing all procedures for the hook.

See also: [`add-hook`](#link6164642d686f6f6b), [`remove-hook`](#link72656d6f76652d686f6f6b).	 [→index](#idx) [→topic](#system)

## `run-hook-internal` : procedure/1 or more {#link72756e2d686f6f6b2d696e7465726e616c}

Usage: `(run-hook-internal hook [args] ...)`

Run all hooks for numeric hook ID `hook` with `args`... as arguments.

See also: [`run-hook`](#link72756e2d686f6f6b).	 [→index](#idx)

**Warning: Internal use only.** [→topic](#system)

## `run-selftest` : procedure/0 {#link72756e2d73656c6674657374}

Usage: `(run-selftest)`

Run a self test of the Z3S5 Lisp system and report errors to standard output.

See also: [`help`](#link68656c70), [`testing`](#link74657374696e67).	 [→index](#idx) [→topic](#system)

## `run-zimage` : procedure/1 or more {#link72756e2d7a696d616765}

Usage: `(run-zimage fi)`

Load the zimage file `fi` and start it at the designated entry point. Raises an error if the zimage version is not compatible or the zimage cannot be run.

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`save-zimage`](#link736176652d7a696d616765), [`zimage-runable?`](#link7a696d6167652d72756e61626c653f), [`zimage-loadable?`](#link7a696d6167652d6c6f616461626c653f).	 [→index](#idx) [→topic](#zimage)

## `sanity-check-help-system` : procedure/0 {#link73616e6974792d636865636b2d68656c702d73797374656d}

Usage: `(sanity-check-help-system)`

Display information about the help system such as missing entries, unnecessary help entries, and faulty help entries.

See also: [`find-missing-help-entries`](#link66696e642d6d697373696e672d68656c702d656e7472696573), [`find-unneeded-help-entries`](#link66696e642d756e6e65656465642d68656c702d656e7472696573), [`find-help-entries-with-incorrect-arity`](#link66696e642d68656c702d656e74726965732d776974682d696e636f72726563742d6172697479).	 [→index](#idx) [→topic](#system)

## `save-zimage` : procedure/1 or more {#link736176652d7a696d616765}

Usage: `(save-zimage min-version info entry-point fi) => int`

Write the current state of the system as a zimage to file `fi`. If the file already exists, it is overwritten. The `min-version` argument designates the minimum system version required to load the zimage. The `info` argument should be a list whose first argument is a human-readable string explaining the purpose of the zimage and remainder is user data. The `entry-point` is either nil or an expression that can be evaluated to start the zimage after it has been loaded with run-zimage.

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`current-zimage`](#link63757272656e742d7a696d616765), [`dump`](#link64756d70), [`run-zimage`](#link72756e2d7a696d616765), [`zimage-loadable?`](#link7a696d6167652d6c6f616461626c653f), [`zimage-runable?`](#link7a696d6167652d72756e61626c653f), [`externalize`](#link65787465726e616c697a65).	 [→index](#idx) [→topic](#zimage)

## `sec+` : procedure/2 {#link7365632b}

Usage: `(sec+ dateli n) => dateli`

Adds `n` seconds to the given date `dateli` in datelist format and returns the new datelist.

See also: [`minute+`](#link6d696e7574652b), [`hour+`](#link686f75722b), [`day+`](#link6461792b), [`week+`](#link7765656b2b), [`month+`](#link6d6f6e74682b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `semver.build` : procedure/1 {#link73656d7665722e6275696c64}

Usage: `(semver.build s) => str`

Return the build part of a semantic versioning string.

See also: [`semver.canonical`](#link73656d7665722e63616e6f6e6963616c), [`semver.major`](#link73656d7665722e6d616a6f72), [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72).	 [→index](#idx) [→topic](#semver)

## `semver.canonical` : procedure/1 {#link73656d7665722e63616e6f6e6963616c}

Usage: `(semver.canonical s) => str`

Return a canonical semver string based on a valid, yet possibly not canonical version string `s.`

See also: [`semver.major`](#link73656d7665722e6d616a6f72).	 [→index](#idx) [→topic](#semver)

## `semver.compare` : procedure/2 {#link73656d7665722e636f6d70617265}

Usage: `(semver.compare s1 s2) => int`

Compare two semantic version strings `s1` and `s2`. The result is 0 if `s1` and `s2` are the same version, -1 if `s1` < `s2` and 1 if `s1` > `s2.`

See also: [`semver.major`](#link73656d7665722e6d616a6f72), [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72).	 [→index](#idx) [→topic](#semver)

## `semver.is-valid?` : procedure/1 {#link73656d7665722e69732d76616c69643f}

Usage: `(semver.is-valid? s) => bool`

Return true if `s` is a valid semantic versioning string, nil otherwise.

See also: [`semver.major`](#link73656d7665722e6d616a6f72), [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72), [`semver.compare`](#link73656d7665722e636f6d70617265).	 [→index](#idx) [→topic](#semver)

## `semver.major` : procedure/1 {#link73656d7665722e6d616a6f72}

Usage: `(semver.major s) => str`

Return the major part of the semantic versioning string.

See also: [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72), [`semver.build`](#link73656d7665722e6275696c64).	 [→index](#idx) [→topic](#semver)

## `semver.major-minor` : procedure/1 {#link73656d7665722e6d616a6f722d6d696e6f72}

Usage: `(semver.major-minor s) => str`

Return the major.minor prefix of a semantic versioning string. For example, (semver.major-minor "v2.1.4") returns "v2.1".

See also: [`semver.major`](#link73656d7665722e6d616a6f72), [`semver.build`](#link73656d7665722e6275696c64).	 [→index](#idx) [→topic](#semver)

## `semver.max` : procedure/2 {#link73656d7665722e6d6178}

Usage: `(semver.max s1 s2) => str`

Canonicalize `s1` and `s2` and return the larger version of them.

See also: [`semver.compare`](#link73656d7665722e636f6d70617265).	 [→index](#idx) [→topic](#semver)

## `semver.prerelease` : procedure/1 {#link73656d7665722e70726572656c65617365}

Usage: `(semver.prerelease s) => str`

Return the prerelease part of a version string, or the empty string if there is none. For example, (semver.prerelease "v2.1.0-pre+build") returns "-pre".

See also: [`semver.build`](#link73656d7665722e6275696c64), [`semver.major`](#link73656d7665722e6d616a6f72), [`semver.major-minor`](#link73656d7665722e6d616a6f722d6d696e6f72).	 [→index](#idx) [→topic](#semver)

## `seq?` : procedure/1 {#link7365713f}

Usage: `(seq? seq) => bool`

Return true if `seq` is a sequence, nil otherwise.

See also: [`list`](#link6c697374), [`array`](#link6172726179), [`string`](#link737472696e67), [`slice`](#link736c696365), [`nth`](#link6e7468).	 [→index](#idx) [→topic](#seq)

## `set` : procedure/3 {#link736574}

Usage: `(set d key value)`

Set `value` for `key` in dict `d.`

See also: [`dict`](#link64696374), [`get`](#link676574), [`get-or-set`](#link6765742d6f722d736574).	 [→index](#idx) [→topic](#dict)

## `set*` : procedure/2 {#link7365742a}

Usage: `(set* d li)`

Set in dict `d` the keys and values in list `li`. The list `li` must be of the form (key-1 value-1 key-2 value-2 ... key-n value-n). This function may be slightly faster than using individual `set` operations.

See also: [`dict`](#link64696374), [`set`](#link736574).	 [→index](#idx) [→topic](#dict)

## `set->list` : procedure/1 {#link7365742d3e6c697374}

Usage: `(set->list s) => li`

Convert set `s` to a list of set elements.

See also: [`list->set`](#link6c6973742d3e736574), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty`](#link7365742d656d707479).	 [→index](#idx) [→topic](#conversion)

## `set-app-metadata` : procedure/7 {#link7365742d6170702d6d65746164617461}

Usage: `(set-app-metadata id name version build icon release? custom)`

Set the metadata of the application to the `id` string, `name` string, `version` string, `build` integer, `icon` resource ID, `release?` bool if this version is a release rather than for testing, and a `custom` dict containing string to string key-value mappings.

See also: [`new-window`](#link6e65772d77696e646f77).	 [→index](#idx) [→topic](#gui)

## `set-canvas-on-typed-key` : procedure/2 {#link7365742d63616e7661732d6f6e2d74797065642d6b6579}

Usage: `(set-canvas-on-typed-key canvas proc)`

Set the procedure `proc` called when a key is typed in `canvas`. `proc` takes two arguments, the first one is a platform-independent key symbol and the second one is a platform- and keyboard-dependent hardware scancode.

See also: [`set-canvas-on-typed-rune`](#link7365742d63616e7661732d6f6e2d74797065642d72756e65), [`add-canvas-shortcut`](#link6164642d63616e7661732d73686f7274637574), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173).	 [→index](#idx) [→topic](#gui)

## `set-canvas-on-typed-rune` : procedure/2 {#link7365742d63616e7661732d6f6e2d74797065642d72756e65}

Usage: `(set-canvas-on-typed-rune canvas proc)`

Set the procedure `proc` called when a rune is typed in `canvas`. `proc` takes one argument, a string containing a single Unicode rune.

See also: [`add-canvas-shortcut`](#link6164642d63616e7661732d73686f7274637574), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`set-canvas-on-typed-key`](#link7365742d63616e7661732d6f6e2d74797065642d6b6579).	 [→index](#idx) [→topic](#gui)

## `set-clipboard-content` : procedure/1 {#link7365742d636c6970626f6172642d636f6e74656e74}

Usage: `(set-clipboard-content str)`

Set the operating system clipboard content to string `str`. This function might raise an error if clipboard access is prohibited by host security settings.

See also: [`get-clipboard-content`](#link6765742d636c6970626f6172642d636f6e74656e74).	 [→index](#idx) [→topic](#gui)

## `set-color` : procedure/1 {#link7365742d636f6c6f72}

Usage: `(set-color sel colorlist)`

Set the color according to `sel` to the color `colorlist` of the form '(r g b a). See `color` for information about `sel.`

See also: [`color`](#link636f6c6f72), [`reset-color`](#link72657365742d636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`with-colors`](#link776974682d636f6c6f7273).	 [→index](#idx) [→topic](#ui)

## `set-complement` : procedure/2 {#link7365742d636f6d706c656d656e74}

Usage: `(set-complement a domain) => set`

Return all elements in `domain` that are not elements of `a.`

See also: [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-difference`](#link7365742d646966666572656e6365), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f), [`set-subset?`](#link7365742d7375627365743f), [`set-equal?`](#link7365742d657175616c3f).	 [→index](#idx) [→topic](#data)

## `set-difference` : procedure/2 {#link7365742d646966666572656e6365}

Usage: `(set-difference a b) => set`

Return the set-theoretic difference of set `a` minus set `b`, i.e., all elements in `a` that are not in `b.`

See also: [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f), [`set-subset?`](#link7365742d7375627365743f), [`set-equal?`](#link7365742d657175616c3f).	 [→index](#idx) [→topic](#data)

## `set-element?` : procedure/2 {#link7365742d656c656d656e743f}

Usage: `(set-element? s elem) => bool`

Return true if set `s` has element `elem`, nil otherwise.

See also: [`make-set`](#link6d616b652d736574), [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx) [→topic](#data)

## `set-empty?` : procedure/1 {#link7365742d656d7074793f}

Usage: `(set-empty? s) => bool`

Return true if set `s` is empty, nil otherwise.

See also: [`make-set`](#link6d616b652d736574), [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f).	 [→index](#idx) [→topic](#data)

## `set-entry-cursor-column` : procedure/2 {#link7365742d656e7472792d637572736f722d636f6c756d6e}

Usage: `(set-entry-cursor-column entry column)`

Set the column position of the cursor in `entry` to integer `column.`

See also: [`get-entry-cursor-pos`](#link6765742d656e7472792d637572736f722d706f73), [`set-entry-cursor-row`](#link7365742d656e7472792d637572736f722d726f77).	 [→index](#idx) [→topic](#gui)

## `set-entry-cursor-row` : procedure/2 {#link7365742d656e7472792d637572736f722d726f77}

Usage: `(set-entry-cursor-row entry row)`

Set the row position of the cursor in `entry` to integer `row.`

See also: [`get-entry-cursor-pos`](#link6765742d656e7472792d637572736f722d706f73), [`set-entry-cursor-column`](#link7365742d656e7472792d637572736f722d636f6c756d6e).	 [→index](#idx) [→topic](#gui)

## `set-entry-min-rows-visible` : procedure/2 {#link7365742d656e7472792d6d696e2d726f77732d76697369626c65}

Usage: `(set-entry-min-rows-visible entry rows)`

Set the minimum number of rows of `entry` that are visible. This ensures that `rows` text rows are visible and is a way of setting the entry's minimum size. Curiously, there is no corresponding set-entry-min-columns-visible function yet.

See also: [`new-entry`](#link6e65772d656e747279).	 [→index](#idx) [→topic](#gui)

## `set-entry-on-change-callback` : procedure/2 {#link7365742d656e7472792d6f6e2d6368616e67652d63616c6c6261636b}

Usage: `(set-entry-on-change-callback entry proc)`

Set the callback of `entry` that is triggered when the entry text changes. `proc` must be a procedure that takes the entry text as string.

See also: [`new-entry`](#link6e65772d656e747279), [`set-entry-cursor-change-callback`](#link7365742d656e7472792d637572736f722d6368616e67652d63616c6c6261636b).	 [→index](#idx) [→topic](#gui)

## `set-entry-on-cursor-change-callback` : procedure/2 {#link7365742d656e7472792d6f6e2d637572736f722d6368616e67652d63616c6c6261636b}

Usage: `(set-entry-cursor-change-callback entry proc)`

Set the cursor change callback of `entry` to `proc`, which is a procedure that takes the entry ID as argument.

See also: [`new-entry`](#link6e65772d656e747279), [`set-entry-on-change-callback`](#link7365742d656e7472792d6f6e2d6368616e67652d63616c6c6261636b).	 [→index](#idx) [→topic](#gui)

## `set-entry-place-holder` : procedure/2 {#link7365742d656e7472792d706c6163652d686f6c646572}

Usage: `(set-entry-place-holder entry str)`

Set the place holder string of `entry` to `str`. This is displayed as a prompt when no text is entered.

See also: [`new-entry`](#link6e65772d656e747279), [`set-entry-text`](#link7365742d656e7472792d74657874).	 [→index](#idx) [→topic](#gui)

## `set-entry-text` : procedure/2 {#link7365742d656e7472792d74657874}

Usage: `(set-entry-text entry str)`

Set the text of `entry` to string `str.`

See also: [`new-entry`](#link6e65772d656e747279), [`set-entry-place-holder`](#link7365742d656e7472792d706c6163652d686f6c646572).	 [→index](#idx) [→topic](#gui)

## `set-entry-text-wrap` : procedure/2 {#link7365742d656e7472792d746578742d77726170}

Usage: `(set-entry-text-wrap entry selector)`

Set or remove the text wrapping of `entry`, which is only relevant for multiline entries. `selector` must be one of '(none break wrap), where 'none indicates no text wrapping, 'break indicates that words are broken without special wrapping algorithm, and 'word means word wrapping.

See also: [`new-entry`](#link6e65772d656e747279).	 [→index](#idx) [→topic](#gui)

## `set-entry-validator` : procedure/2 {#link7365742d656e7472792d76616c696461746f72}

Usage: `(set-entry-validator entry validator)`

Set the `validator` of `entry`. A validator must be created first from a special procedure or a regular expression.

See also: [`new-entry`](#link6e65772d656e747279), [`new-validator`](#link6e65772d76616c696461746f72), [`new-combined-string-validator`](#link6e65772d636f6d62696e65642d737472696e672d76616c696461746f72), [`new-time-validator`](#link6e65772d74696d652d76616c696461746f72), [`new-regexp-validator`](#link6e65772d7265676578702d76616c696461746f72), [`validate-object`](#link76616c69646174652d6f626a656374).	 [→index](#idx) [→topic](#gui)

## `set-equal?` : procedure/2 {#link7365742d657175616c3f}

Usage: `(set-equal? a b) => bool`

Return true if `a` and `b` contain the same elements.

See also: [`set-subset?`](#link7365742d7375627365743f), [`list->set`](#link6c6973742d3e736574), [`set-element?`](#link7365742d656c656d656e743f), [`set->list`](#link7365742d3e6c697374), [`set-union`](#link7365742d756e696f6e), [`set-difference`](#link7365742d646966666572656e6365), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx) [→topic](#data)

## `set-help-topic-info` : procedure/3 {#link7365742d68656c702d746f7069632d696e666f}

Usage: `(set-help-topic-info topic header info)`

Set a human-readable information entry for help `topic` with human-readable `header` and `info` strings.

See also: [`defhelp`](#link64656668656c70), [`help-topic-info`](#link68656c702d746f7069632d696e666f).	 [→index](#idx) [→topic](#help)

## `set-intersection` : procedure/2 {#link7365742d696e74657273656374696f6e}

Usage: `(set-intersection a b) => set`

Return the intersection of sets `a` and `b`, i.e., the set of elements that are both in `a` and in `b.`

See also: [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f), [`set-subset?`](#link7365742d7375627365743f), [`set-equal?`](#link7365742d657175616c3f).	 [→index](#idx) [→topic](#data)

## `set-label-text` : procedure/2 {#link7365742d6c6162656c2d74657874}

Usage: `(set-label-text label str)`

Sets the text of `label` to string `str`. This might resize the label depending on the layout in which the label is put.

See also: [`get-label-text`](#link6765742d6c6162656c2d74657874), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `set-menu-item-checked` : procedure/2 {#link7365742d6d656e752d6974656d2d636865636b6564}

Usage: `(set-menu-item-checked item checked?)`

Set the menu item check mark display if `checked?` is non-nil, remove it otherwise.

See also: [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`menu-item-disabled?`](#link6d656e752d6974656d2d64697361626c65643f), [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx) [→topic](#gui)

## `set-menu-item-disabled` : procedure/2 {#link7365742d6d656e752d6974656d2d64697361626c6564}

Usage: `(set-menu-item-disabled item disabled?)`

Disable the menu item if `disabled?` is non-nil, enable it otherwise.

See also: [`menu-item-disabled?`](#link6d656e752d6974656d2d64697361626c65643f), [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-label`](#link7365742d6d656e752d6974656d2d6c6162656c), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx) [→topic](#gui)

## `set-menu-item-label` : procedure/2 {#link7365742d6d656e752d6974656d2d6c6162656c}

Usage: `(set-menu-item-label item str)`

Set the label of menu `item` to `str.`

See also: [`get-menu-item-label`](#link6765742d6d656e752d6974656d2d6c6162656c), [`set-menu-item-disabled`](#link7365742d6d656e752d6974656d2d64697361626c6564), [`menu-item-disabled`](#link6d656e752d6974656d2d64697361626c6564), [`set-menu-item-checked`](#link7365742d6d656e752d6974656d2d636865636b6564), [`menu-item-checked?`](#link6d656e752d6974656d2d636865636b65643f), [`new-menu*`](#link6e65772d6d656e752a), [`new-menu`](#link6e65772d6d656e75), [`new-menu-item`](#link6e65772d6d656e752d6974656d), [`new-menu-item-separator`](#link6e65772d6d656e752d6974656d2d736570617261746f72).	 [→index](#idx) [→topic](#gui)

## `set-object-on-validation-change-callback` : procedure/2 {#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b}

Usage: `(set-object-on-validation-change-callback obj proc)`

Set a validatable object's `obj` validation change callback, which is called when the object's validation changes. The callback `proc` takes a string or nil as argument. When it is nil, the validation was successful. When it is a string, then the validation failed with the string as reason. This can be used to track validation changes of any validatable object (such as a text entry) to e.g. display custom messages or icons when the validation fails or succeeds.

See also: [`validate-object`](#link76616c69646174652d6f626a656374), [`new-validator`](#link6e65772d76616c696461746f72), [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72).	 [→index](#idx) [→topic](#gui)

## set-permissions : nil {#link7365742d7065726d697373696f6e73}

Usage: `(set-permissions li)`

Set the permissions for the current interpreter. This will trigger an error when the permission cannot be set due to a security violation. Generally, permissions can only be downgraded (made more stringent) and never relaxed. See the information for `permissions` for an overview of symbolic flags.

See also: [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f), [`when-permission`](#link7768656e2d7065726d697373696f6e), [`sys`](#link737973).	 [→index](#idx) [→topic](#system)

## `set-progress-bar` : procedure/1 or more {#link7365742d70726f67726573732d626172}

Usage: `(set-progress-bar bar value [selector value])`

Set the value of progress-bar `bar` as follows. If a single number is provided, then the current value of the progress-bar is set to this number. If a selector symbol is provided, then if it is 'value, the progress-bar value is set to the following number, if it is 'max or 'min, then the progress-bar maximum or minimum values are set to the respective following number. If it is 'formatter, then the following value must be a procedure that takes the progress-bar ID as argument and returns a string that represents the display of the progress-bar at the given time.

See also: [`get-progress-bar-value`](#link6765742d70726f67726573732d6261722d76616c7565), [`new-progress-bar`](#link6e65772d70726f67726573732d626172), [`new-infinite-progress-bar`](#link6e65772d696e66696e6974652d70726f67726573732d626172).	 [→index](#idx) [→topic](#gui)

## `set-scroll-offset` : procedure/2 {#link7365742d7363726f6c6c2d6f6666736574}

Usage: `(set-scroll-offset scroll li)`

Set the `scroll` offset to `li`, which is a position of the form (x y) where `x` and `y` are floats. If you don't want to change `x` or `y` respectively, you need to use `get-scroll-offset` first to get the value that you don't want to change, and construct the position from that.

See also: [`get-scroll-offset`](#link6765742d7363726f6c6c2d6f6666736574), [`new-scroll`](#link6e65772d7363726f6c6c), [`new-hscroll`](#link6e65772d687363726f6c6c), [`new-vscroll`](#link6e65772d767363726f6c6c).	 [→index](#idx) [→topic](#gui)

## `set-select-options` : procedure/2 {#link7365742d73656c6563742d6f7074696f6e73}

Usage: `(set-select-options select li)`

Set the options of the given `select` widget, which can be obtained using `new-choice` with selector 'select.

See also: [`new-choice`](#link6e65772d63686f696365).	 [→index](#idx) [→topic](#gui)

## `set-slider-value` : procedure/2 {#link7365742d736c696465722d76616c7565}

Usage: `(set-slider-value slider fl)`

Set the value of `slider` to float `fl.`

See also: [`new-slider`](#link6e65772d736c69646572).	 [→index](#idx) [→topic](#gui)

## `set-split-offset` : procedure/2 {#link7365742d73706c69742d6f6666736574}

Usage: `(set-split-offset split offset)`

Set the offset of `split` to float `offset` between 0.0 and 1.0. `offset` indicates the percentage between the objects shown in the split. If `offset` is 0.0, then only the second object is shown, if it is 1.0 then only the first object is shown.

See also: [`new-vsplit`](#link6e65772d7673706c6974), [`new-hsplit`](#link6e65772d6873706c6974).	 [→index](#idx) [→topic](#gui)

## `set-subset?` : procedure/2 {#link7365742d7375627365743f}

Usage: `(set-subset? a b) => bool`

Return true if `a` is a subset of `b`, nil otherwise.

See also: [`set-equal?`](#link7365742d657175616c3f), [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-difference`](#link7365742d646966666572656e6365), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx) [→topic](#data)

## `set-text-alignment` : procedure/2 {#link7365742d746578742d616c69676e6d656e74}

Usage: `(set-text-alignment text sym)`

Set the alignment of `text` to `sym`, which must be one of '(leading center trailing).

See also: [`new-text`](#link6e65772d74657874), [`set-text-size`](#link7365742d746578742d73697a65), [`set-text-style`](#link7365742d746578742d7374796c65).	 [→index](#idx) [→topic](#gui)

## `set-text-grid-cell` : procedure/4 {#link7365742d746578742d677269642d63656c6c}

Usage: `(set-text-grid-cell grid row column li)`

Set the text `grid` cell at `row` and `column` (both 0-indexed) to the list `li`, where `li` must consist of a unicode string containing one rune and a valid grid style list.

See also: [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77).	 [→index](#idx) [→topic](#gui)

## `set-text-grid-row` : procedure/3 {#link7365742d746578742d677269642d726f77}

Usage: `(set-text-grid-row grid row row-spec)`

Set the `row` of `grid` to the given `row-spec`, which is a list containing an array of grid cells like in the return value of `get-text-grid-row` and a grid style for the row as a whole.

See also: [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765).	 [→index](#idx) [→topic](#gui)

## `set-text-grid-row-style` : procedure/3 {#link7365742d746578742d677269642d726f772d7374796c65}

Usage: `(set-text-grid-row-style grid row style)`

Set the style of text `grid` at `row` to the given grid `style.`

See also: [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765).	 [→index](#idx) [→topic](#gui)

## `set-text-grid-rune` : procedure/4 {#link7365742d746578742d677269642d72756e65}

Usage: `(set-text-grid-rune grid row column str)`

Set the rune of `grid` at `row` and `column` to the unicode glyph in string `str.`

See also: [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c).	 [→index](#idx) [→topic](#gui)

## `set-text-grid-show-line-numbers` : procedure/2 {#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273}

Usage: `(set-text-grid-show-line-numbers grid show?)`

Set whether `grid` shows line numbers. If `show?` is not nil, then line numbers are shown, otherwise they are not shown.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx) [→topic](#gui)

## `set-text-grid-show-whitespace` : procedure/2 {#link7365742d746578742d677269642d73686f772d77686974657370616365}

Usage: `(set-text-grid-show-whitespace grid show?)`

Set whether `grid` shows whitespace characters. If `show?` is not nil, then whitespace characters are shown, otherwise they are not shown.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx) [→topic](#gui)

## `set-text-grid-style` : procedure/4 {#link7365742d746578742d677269642d7374796c65}

Usage: `(set-text-grid-style grid row column style)`

Set the grid style of `grid` at `row` and `column` to the a-list `style.`

See also: [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765).	 [→index](#idx) [→topic](#gui)

## `set-text-grid-style-range` : procedure/6 {#link7365742d746578742d677269642d7374796c652d72616e6765}

Usage: `(set-text-grid-style-range grid start-row start-column end-row end-column style)`

Set the grid style of `grid` starting at `start-row` and `start-column` and ending at `end-row` and `end-column` (all inclusive) to the grid `style.`

See also: [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65).	 [→index](#idx) [→topic](#gui)

## `set-text-grid-tab-width` : procedure/2 {#link7365742d746578742d677269642d7461622d7769647468}

Usage: `(set-text-grid-tab-width grid width)`

Set the tabulator width of `grid` to integer `width` space characters.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx) [→topic](#gui)

## `set-text-grid-text` : procedure/2 {#link7365742d746578742d677269642d74657874}

Usage: `(set-text-grid-text grid str)`

Set the text of the text `grid` to the given `str.`

See also: [`get-text-grid-text`](#link6765742d746578742d677269642d74657874), [`new-text-grid`](#link6e65772d746578742d67726964), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77).	 [→index](#idx) [→topic](#gui)

## `set-text-size` : procedure/2 {#link7365742d746578742d73697a65}

Usage: `(set-text-size text size)`

Set the size of `text` to float `size.`

See also: [`new-text`](#link6e65772d74657874), [`set-text-alignment`](#link7365742d746578742d616c69676e6d656e74), [`set-text-style`](#link7365742d746578742d7374796c65).	 [→index](#idx) [→topic](#gui)

## `set-text-style` : procedure/2 {#link7365742d746578742d7374796c65}

Usage: `(set-text-style text li`

Set the style of `text` to the specification in list `li`, which must contain symbols in '(bold italic monospace symbol tab-width). If a symbol in the list is `tab-width`, it must be followed by an integer. `bold` sets boldface, `italic` makes the style italic, `monospace` selects the monospace/typewriter font, and `symbol` selects the `symbol` font. `tab-width` followed by an integer sets the width of tabulator in terms of the number of space characters.

See also: [`new-text`](#link6e65772d74657874), [`set-text-alignment`](#link7365742d746578742d616c69676e6d656e74), [`set-text-size`](#link7365742d746578742d73697a65).	 [→index](#idx) [→topic](#gui)

## `set-union` : procedure/2 {#link7365742d756e696f6e}

Usage: `(set-union a b) => set`

Return the union of sets `a` and `b` containing all elements that are in `a` or in `b` (or both).

See also: [`list->set`](#link6c6973742d3e736574), [`set->list`](#link7365742d3e6c697374), [`make-set`](#link6d616b652d736574), [`set-element?`](#link7365742d656c656d656e743f), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set?`](#link7365743f), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx) [→topic](#data)

## `set-volume` : procedure/1 {#link7365742d766f6c756d65}

Usage: `(set-volume fl)`

Set the master volume for all sound to `fl`, a value between 0.0 and 1.0.

See also: [`beep`](#link62656570).	 [→index](#idx) [→topic](#sound)

## `set-window-content` : procedure/2 {#link7365742d77696e646f772d636f6e74656e74}

Usage: `(set-window-content window canvas-object)`

Set the main content of the window, which must be an existing canvas object such as a widget or container with layout.

See also: [`get-window-content`](#link6765742d77696e646f772d636f6e74656e74), [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`new-window`](#link6e65772d77696e646f77).	 [→index](#idx) [→topic](#gui)

## `set-window-full-screen` : procedure/2 {#link7365742d77696e646f772d66756c6c2d73637265656e}

Usage: `(set-window-full-screen window full-screen?)`

If `full-screen?` is not nil, then `window` is set to full screen mode, otherwise the window is set to normal mode. In full screen mode the window is centered and fills the whole screen of the main monitor (multiple monitors are currently not supported).

See also: [`window-full-screen?`](#link77696e646f772d66756c6c2d73637265656e3f), [`center-window-on-screen`](#link63656e7465722d77696e646f772d6f6e2d73637265656e).	 [→index](#idx) [→topic](#gui)

## `set-window-icon` : procedure/1 {#link7365742d77696e646f772d69636f6e}

Usage: `(set-window-icon window resource)`

Set the icon of `window` to the given icon `resource`. Setting the icon does not guarantee that it is displayed, since this is platform-dependent.

See also: [`get-window-icon`](#link6765742d77696e646f772d69636f6e), [`new-icon`](#link6e65772d69636f6e), [`theme-icon`](#link7468656d652d69636f6e).	 [→index](#idx) [→topic](#gui)

## `set-window-main-menu` : procedure/2 {#link7365742d77696e646f772d6d61696e2d6d656e75}

Usage: `(set-window-main-menu window main-menu)`

Set the main menu of `window` to `main-menu.`

See also: [`new-main-menu`](#link6e65772d6d61696e2d6d656e75), [`new-menu`](#link6e65772d6d656e75), [`new-menu*`](#link6e65772d6d656e752a).	 [→index](#idx) [→topic](#gui)

## `set-window-on-close-callback` : procedure/2 {#link7365742d77696e646f772d6f6e2d636c6f73652d63616c6c6261636b}

Usage: `(set-window-on-close-callback window proc)`

Sets the close callback of `window`. `proc` must be a function that takes no arguments and is evaluated when the window is closed.

See also: [`show-window`](#link73686f772d77696e646f77), [`close-window`](#link636c6f73652d77696e646f77), [`hide-window`](#link686964652d77696e646f77).	 [→index](#idx) [→topic](#gui)

## `set-window-padded` : procedure/2 {#link7365742d77696e646f772d706164646564}

Usage: `(set-window-padded window padded?)`

If `padded?` is not nil, then `window` is set to being padded. This is the default of new window. If `padded?` is nil, then the window's padding is removed, which means that the whole content area of the window can be filled with user interface elements and draw commands. This would be used for a game display, for instance.

See also: [`window-padded?`](#link77696e646f772d7061646465643f), [`set-window-full-screen`](#link7365742d77696e646f772d66756c6c2d73637265656e), [`window-full-screen?`](#link77696e646f772d66756c6c2d73637265656e3f), [`center-window-on-screen`](#link63656e7465722d77696e646f772d6f6e2d73637265656e).	 [→index](#idx) [→topic](#gui)

## `set-window-size` : procedure/2 {#link7365742d77696e646f772d73697a65}

Usage: `(set-window-size window width height)`

Set the size of `window` to `width` and `height` as floats. Sizes and positions are generally given as floats whose accuracy is only guaranteed to 32 bit.

See also: [`new-window`](#link6e65772d77696e646f77), [`show-window`](#link73686f772d77696e646f77), [`hide-window`](#link686964652d77696e646f77).	 [→index](#idx) [→topic](#gui)

## `set-window-title` : procedure/2 {#link7365742d77696e646f772d7469746c65}

Usage: `(set-window-title window title)`

Set the title of `window` to string `title.`

See also: [`get-window-title`](#link6765742d77696e646f772d7469746c65).	 [→index](#idx) [→topic](#gui)

## `set-zedit-config` : procedure/3 {#link7365742d7a656469742d636f6e666967}

Usage: `(set-zedit-config zedit sel b)`

Set a configuration property of the given `zedit` to `b`. The selector `sel` can be one of the following: show-line-numbers? - show the line number display if true, show-whitespace? - show whitespace characters if true, line-wrap? - wrap lines to the size of the widget automatically, soft-wrap? - do not end lines in a hard line feed when line wrapping (default), draw-caret? - draw the caret if true, supress caret drawing if false, highlight-parens? - automatically highlight the matching opening paren while typing and moving the caret after a closing paren, highlight-paren-range? - automatically highlight the range of characters between the opening and closing paren when hightlight-parens? is true, get-word-at-left? - returns word left of caret if true, otherwise words are only triggered when the caret is on them, liberal-get-word-at? - words are parsed more like Z3S5 Lisp symbols, i.e., including all kinds of delimiters except for parentheses.

See also: [`new-zedit`](#link6e65772d7a65646974), [`get-zedit-config`](#link6765742d7a656469742d636f6e666967).	 [→index](#idx) [→topic](#gui)

## set-zedit-event-handler : nil {#link7365742d7a656469742d6576656e742d68616e646c6572}

Usage: `(set-zedit-event-handler editor sel proc)`

Set the event handler for `sel` and the given `event` to `proc`. The handler `proc` is a function that takes an event selector and an editor as argument. The selector `sel` must be one of '(caret-move word-change).

See also: [`new-zedit`](#link6e65772d7a65646974), [`remove-zedit-event-handler`](#link72656d6f76652d7a656469742d6576656e742d68616e646c6572).	 [→index](#idx) [→topic](#gui)

## `set-zedit-line-number-style` : procedure/2 {#link7365742d7a656469742d6c696e652d6e756d6265722d7374796c65}

Usage: `(set-zedit-line-number-style zedit li)`

Set the style of the line number display in `zedit`, even when the line number display is off. The style list is for en editor style, an a-list containing boolean keys in '(bold italic monospace) followed by nil or t, and color keys 'text-color and 'background-color followed by a 16-bit (r g b a) color list.

See also: [`new-zedit`](#link6e65772d7a65646974), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65).	 [→index](#idx) [→topic](#gui)

## `set-zedit-text` : procedure/2 {#link7365742d7a656469742d74657874}

Usage: `(set-zedit-text zedit str)`

Sets the text in the zedit editor to `str.`

See also: [`new-zedit`](#link6e65772d7a65646974), [`get-zedit-text`](#link6765742d7a656469742d74657874).	 [→index](#idx) [→topic](#gui)

## `set-zedit-top-line` : procedure/2 {#link7365742d7a656469742d746f702d6c696e65}

Usage: `(set-zedit-top-line zedit n)`

Set the top visible line in `zedit` to `n`, scrolling the editor accordingly.

See also: [`get-zedit-top-line`](#link6765742d7a656469742d746f702d6c696e65).	 [→index](#idx) [→topic](#gui)

## `set?` : procedure/1 {#link7365743f}

Usage: `(set? x) => bool`

Return true if `x` can be used as a set, nil otherwise.

See also: [`list->set`](#link6c6973742d3e736574), [`make-set`](#link6d616b652d736574), [`set->list`](#link7365742d3e6c697374), [`set-element?`](#link7365742d656c656d656e743f), [`set-union`](#link7365742d756e696f6e), [`set-intersection`](#link7365742d696e74657273656374696f6e), [`set-complement`](#link7365742d636f6d706c656d656e74), [`set-difference`](#link7365742d646966666572656e6365), [`set-empty?`](#link7365742d656d7074793f).	 [→index](#idx) [→topic](#data)

## `setcar` : procedure/2 {#link736574636172}

Usage: `(setcar li elem) => li`

Mutate `li` such that its car is `elem`. Same as rplaca.

See also: [`rplaca`](#link72706c616361), [`rplacd`](#link72706c616364), [`setcdr`](#link736574636472).	 [→index](#idx) [→topic](#lisp)

## `setcdr` : procedure/1 {#link736574636472}

Usage: `(setcdr li1 li2) => li`

Mutate `li1` such that its cdr is `li2`. Same as rplacd.

See also: [`rplacd`](#link72706c616364), [`rplaca`](#link72706c616361), [`setcar`](#link736574636172).	 [→index](#idx) [→topic](#lisp)

## `setprop` : procedure/3 {#link73657470726f70}

Usage: `(setprop obj slot value)`

Set property `slot` in `obj` to `value`. An error occurs if the object does not have a property with that name.

See also: [`new`](#link6e6577), [`isa?`](#link6973613f), [`prop`](#link70726f70), [`object?`](#link6f626a6563743f), [`class-name`](#link636c6173732d6e616d65), [`supers`](#link737570657273), [`props`](#link70726f7073), [`methods`](#link6d6574686f6473), [`has-prop?`](#link6861732d70726f703f).	 [→index](#idx) [→topic](#oop)

## `shorten` : procedure/2 {#link73686f7274656e}

Usage: `(shorten s n) => str`

Shorten string `s` to length `n` in a smart way if possible, leave it untouched if the length of `s` is smaller than `n.`

See also: [`substr`](#link737562737472).	 [→index](#idx) [→topic](#str)

## `show-color-picker` : procedure/3 or more {#link73686f772d636f6c6f722d7069636b6572}

Usage: `(show-color-picker title message proc [win]`

Show a color picker dialog with given `title` and `message`. The callback `proc` takes a color list and is called when a color is chosen. The optional `win` argument specifies the parent window.

See also: [`nrgba`](#link6e72676261), [`nrgba64`](#link6e726762613634), [`color`](#link636f6c6f72), [`the-color`](#link7468652d636f6c6f72), [`theme-color`](#link7468656d652d636f6c6f72).	 [→index](#idx) [→topic](#gui)

## `show-confirm` : procedure/4 {#link73686f772d636f6e6669726d}

Usage: `(show-confirm title message proc win)`

Show a simple confirmation dialog with Yes and No as options and `title` and `message`. The callback `proc` takes a bool that is true if the dialog has been confirmed, nil otherwise. The `win` argument specifies the parent window.

See also: [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d), [`show-custom`](#link73686f772d637573746f6d), [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73), [`show-information`](#link73686f772d696e666f726d6174696f6e), [`show-form`](#link73686f772d666f726d).	 [→index](#idx) [→topic](#gui)

## `show-custom` : procedure/4 {#link73686f772d637573746f6d}

Usage: `(show-custom title dismiss content win)`

Show a custom info dialog, where `title` is the title of the dialog, `dismiss` is a string for the button text to dismiss the dialog, `content` is a valid canvas object ID for the content of the dialog such as the ID of an entry or form, and `win` is the parent window.

See also: [`show-confirm`](#link73686f772d636f6e6669726d), [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d), [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73), [`show-information`](#link73686f772d696e666f726d6174696f6e), [`show-form`](#link73686f772d666f726d).	 [→index](#idx) [→topic](#gui)

## `show-custom-confirm` : procedure/6 {#link73686f772d637573746f6d2d636f6e6669726d}

Usage: `(show-custom-confirm title confirm dismiss content proc win)`

Show a custom confirm dialog, where `title` is the dialog's title, `confirm` is the text of the confirm button, `dismiss` is the text of the dismiss button, content is a valid canvas object ID for the content of the dialog such as an ID for an entry or form, `proc` is a callback procedure that takes one argument that is true if the dialog was confirmed, nil otherwise, and `win` is the parent window.

See also: [`show-confirm`](#link73686f772d636f6e6669726d), [`show-custom`](#link73686f772d637573746f6d), [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73), [`show-information`](#link73686f772d696e666f726d6174696f6e), [`show-form`](#link73686f772d666f726d).	 [→index](#idx) [→topic](#gui)

## `show-custom-without-buttons` : procedure/3 {#link73686f772d637573746f6d2d776974686f75742d627574746f6e73}

Usage: `(show-custom-without-buttons title content win)`

Show a custom dialog without buttons, where `title` is the dialog's title, `content` is a valid canvas object ID for the content of the dialog such as an ID for an entry or form, and `win` is the parent window.

See also: [`show-custom`](#link73686f772d637573746f6d), [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d), [`show-confirm`](#link73686f772d636f6e6669726d), [`show-information`](#link73686f772d696e666f726d6174696f6e), [`show-form`](#link73686f772d666f726d).	 [→index](#idx) [→topic](#gui)

## `show-file-open` : procedure/2 {#link73686f772d66696c652d6f70656e}

Usage: `(show-file-open proc win)`

Show a standard file open dialog that allows the user to select an existing file. The procedure `proc` takes two arguments. The first argument is a readable port if the user chose a file, nil if the user canceled. The second argument is nil if no error occurred, and an error string if an error occurred. Notice that the port can be not-nil and the error string can be non-empty at the same time, indicating that a file for opening was chosen but some I/O error occurred.

See also: [`show-file-open`](#link73686f772d66696c652d6f70656e).	 [→index](#idx) [→topic](#gui)

## `show-file-save` : procedure/2 {#link73686f772d66696c652d73617665}

Usage: `(show-file-save proc win)`

Show a standard file save dialog that allows the user to chose a save file. If the chosen file already exists, the user is asked whether they want to overwrite the file. The procedure `proc` takes two arguments. The first argument is a writeable port if the user chose a file, nil if the user canceled. The second argument is nil if no error occurred, and an error string if an error occurred. Notice that the port can be not-nil and the error string can be non-empty at the same time, indicating that a file for saving was chosen but some I/O error occurred.

See also: [`show-file-open`](#link73686f772d66696c652d6f70656e).	 [→index](#idx) [→topic](#gui)

## `show-folder-open` : procedure/2 {#link73686f772d666f6c6465722d6f70656e}

Usage: `(show-folder-open proc win)`

Show an open folder dialog to select a folder. The procedure `proc` takes two arguments. The first argument is an URI string if a folder was chosen, nil otherwise. The second argument is nil if no error occurred, and an error string if an error occurred. Notice that URI can be not-nil and the error string can be non-empty at the same time, indicating that a folder was chosen but some I/O error occurred.

See also: [`list-uri`](#link6c6973742d757269), [`listable-uri?`](#link6c69737461626c652d7572693f), [`uri?`](#link7572693f).	 [→index](#idx) [→topic](#gui)

## `show-form` : procedure/6 {#link73686f772d666f726d}

Usage: `(show-form title confirm dismiss li proc win)`

Show a list of form items whose ID must reside in `li` and check these items are validated. The `title` is the dialog's title, `confirm` is the text of the confirm button, `dismiss` the text of the dismiss button, `proc` is a procedure that takes a boolean argument that is true if all form items have been validated and confirmed, nil otherwise, and `win` is the parent window. The form items in `li` must be widgets with validators that can be added to a form.

See also: [`show-custom`](#link73686f772d637573746f6d), [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73), [`show-confirm`](#link73686f772d636f6e6669726d), [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d).	 [→index](#idx) [→topic](#gui)

## `show-information` : procedure/3 {#link73686f772d696e666f726d6174696f6e}

Usage: `(show-information title message win)`

Show a dialog with `title` and information `message` strings, where `win` is the parent window.

See also: [`show-custom`](#link73686f772d637573746f6d), [`show-custom-without-buttons`](#link73686f772d637573746f6d2d776974686f75742d627574746f6e73), [`show-confirm`](#link73686f772d636f6e6669726d), [`show-custom-confirm`](#link73686f772d637573746f6d2d636f6e6669726d), [`show-form`](#link73686f772d666f726d).	 [→index](#idx) [→topic](#gui)

## `show-object` : procedure/1 {#link73686f772d6f626a656374}

Usage: `(show-object obj)`

Show the canvas object `obj.`

See also: [`disable-object`](#link64697361626c652d6f626a656374), [`enable-object`](#link656e61626c652d6f626a656374), [`hide-object`](#link686964652d6f626a656374), [`object-disabled?`](#link6f626a6563742d64697361626c65643f), [`move-object`](#link6d6f76652d6f626a656374), [`resize-object`](#link726573697a652d6f626a656374), [`get-object-size`](#link6765742d6f626a6563742d73697a65), [`get-object-min-size`](#link6765742d6f626a6563742d6d696e2d73697a65), [`get-object-position`](#link6765742d6f626a6563742d706f736974696f6e), [`object-visible?`](#link6f626a6563742d76697369626c653f), [`refresh-object`](#link726566726573682d6f626a656374), [`new-entry`](#link6e65772d656e747279), [`new-label`](#link6e65772d6c6162656c).	 [→index](#idx) [→topic](#gui)

## `sleep` : procedure/1 {#link736c656570}

Usage: `(sleep ms)`

Halt the current task execution for `ms` milliseconds.

See also: [`sleep-ns`](#link736c6565702d6e73), [`time`](#link74696d65), [`now`](#link6e6f77), [`now-ns`](#link6e6f772d6e73).	 [→index](#idx) [→topic](#system)

## `sleep-ns` : procedure/1 {#link736c6565702d6e73}

Usage: `(sleep-ns n`

Halt the current task execution for `n` nanoseconds.

See also: [`sleep`](#link736c656570), [`time`](#link74696d65), [`now`](#link6e6f77), [`now-ns`](#link6e6f772d6e73).	 [→index](#idx) [→topic](#system)

## `slice` : procedure/3 {#link736c696365}

Usage: `(slice seq low high) => seq`

Return the subsequence of `seq` starting from `low` inclusive and ending at `high` exclusive. Sequences are 0-indexed.

See also: [`list`](#link6c697374), [`array`](#link6172726179), [`string`](#link737472696e67), [`nth`](#link6e7468), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#seq)

## `sort` : procedure/2 {#link736f7274}

Usage: `(sort li proc) => li`

Sort the list `li` by the given less-than procedure `proc`, which takes two arguments and returns true if the first one is less than the second, nil otheriwse.

See also: [`array-sort`](#link61727261792d736f7274).	 [→index](#idx) [→topic](#lisp)

## sort-symbols : nil {#link736f72742d73796d626f6c73}

Usage: `(sort-symbols li) => list`

Sort the list of symbols `li` alphabetically.

See also: [`out`](#link6f7574), [`dp`](#link6470), [`du`](#link6475), [`dump`](#link64756d70).	 [→index](#idx) [→topic](#lisp)

## `spaces` : procedure/1 {#link737061636573}

Usage: `(spaces n) => str`

Create a string consisting of `n` spaces.

See also: [`strbuild`](#link7374726275696c64), [`strleft`](#link7374726c656674), [`strright`](#link7374727269676874).	 [→index](#idx) [→topic](#str)

## `stack-empty?` : procedure/1 {#link737461636b2d656d7074793f}

Usage: `(queue-empty? s) => bool`

Return true if the stack `s` is empty, nil otherwise.

See also: [`make-stack`](#link6d616b652d737461636b), [`stack?`](#link737461636b3f), [`push!`](#link7075736821), [`pop!`](#link706f7021), [`stack-len`](#link737461636b2d6c656e), [`glance`](#link676c616e6365).	 [→index](#idx) [→topic](#data)

## `stack-len` : procedure/1 {#link737461636b2d6c656e}

Usage: `(stack-len s) => int`

Return the length of the stack `s.`

See also: [`make-queue`](#link6d616b652d7175657565), [`queue?`](#link71756575653f), [`enqueue!`](#link656e717565756521), [`dequeue!`](#link6465717565756521), [`glance`](#link676c616e6365), [`queue-len`](#link71756575652d6c656e).	 [→index](#idx)

**Warning: Be advised that this is of limited use in some concurrent contexts, since the length of the queue might have changed already once you've obtained it!** [→topic](#data)

## `stack?` : procedure/1 {#link737461636b3f}

Usage: `(stack? q) => bool`

Return true if `q` is a stack, nil otherwise.

See also: [`make-stack`](#link6d616b652d737461636b), [`push!`](#link7075736821), [`pop!`](#link706f7021), [`stack-empty?`](#link737461636b2d656d7074793f), [`stack-len`](#link737461636b2d6c656e), [`glance`](#link676c616e6365).	 [→index](#idx) [→topic](#data)

## `str+` : procedure/0 or more {#link7374722b}

Usage: `(str+ [s] ...) => str`

Append all strings given to the function.

See also: [`str?`](#link7374723f).	 [→index](#idx) [→topic](#str)

## `str->array` : procedure/1 {#link7374722d3e6172726179}

Usage: `(str->array s) => array`

Return the string `s` as an array of unicode glyph integer values.

See also: [`array->str`](#link61727261792d3e737472).	 [→index](#idx) [→topic](#conversion)

## `str->blob` : procedure/1 {#link7374722d3e626c6f62}

Usage: `(str->blob s) => blob`

Convert string `s` into a blob.

See also: [`blob->str`](#link626c6f622d3e737472).	 [→index](#idx) [→topic](#conversion)

## `str->char` : procedure/1 {#link7374722d3e63686172}

Usage: `(str->char s)`

Return the first character of `s` as unicode integer.

See also: [`char->str`](#link636861722d3e737472).	 [→index](#idx) [→topic](#conversion)

## `str->chars` : procedure/1 {#link7374722d3e6368617273}

Usage: `(str->chars s) => array`

Convert the UTF-8 string `s` into an array of UTF-8 rune integers. An error may occur if the string is not a valid UTF-8 string.

See also: [`runes->str`](#link72756e65732d3e737472), [`str->char`](#link7374722d3e63686172), [`char->str`](#link636861722d3e737472).	 [→index](#idx) [→topic](#conversion)

## `str->expr` : procedure/0 or more {#link7374722d3e65787072}

Usage: `(str->expr s [default]) => any`

Convert a string `s` into a Lisp expression. If `default` is provided, it is returned if an error occurs, otherwise an error is raised.

See also: [`expr->str`](#link657870722d3e737472), [`str->expr*`](#link7374722d3e657870722a), [`openstr`](#link6f70656e737472), [`externalize`](#link65787465726e616c697a65), [`internalize`](#link696e7465726e616c697a65).	 [→index](#idx) [→topic](#conversion)

## `str->expr*` : procedure/0 or more {#link7374722d3e657870722a}

Usage: `(str->expr* s [default]) => li`

Convert a string `s` into a list consisting of the Lisp expressions in `s`. If `default` is provided, then this value is put in the result list whenever an error occurs. Otherwise an error is raised. Notice that it might not always be obvious what expression in `s` triggers an error, since this hinges on the way the internal expession parser works.

See also: [`str->expr`](#link7374722d3e65787072), [`expr->str`](#link657870722d3e737472), [`openstr`](#link6f70656e737472), [`internalize`](#link696e7465726e616c697a65), [`externalize`](#link65787465726e616c697a65).	 [→index](#idx) [→topic](#conversion)

## `str->list` : procedure/1 {#link7374722d3e6c697374}

Usage: `(str->list s) => list`

Return the sequence of numeric chars that make up string `s.`

See also: [`str->array`](#link7374722d3e6172726179), [`list->str`](#link6c6973742d3e737472), [`array->str`](#link61727261792d3e737472), [`chars`](#link6368617273).	 [→index](#idx) [→topic](#conversion)

## `str->sym` : procedure/1 {#link7374722d3e73796d}

Usage: `(str->sym s) => sym`

Convert a string into a symbol.

See also: [`sym->str`](#link73796d2d3e737472), [`intern`](#link696e7465726e), [`make-symbol`](#link6d616b652d73796d626f6c).	 [→index](#idx) [→topic](#conversion)

## `str-count-substr` : procedure/2 {#link7374722d636f756e742d737562737472}

Usage: `(str-count-substr s1 s2) => int`

Count the number of non-overlapping occurrences of substring `s2` in string `s1.`

See also: [`str-replace`](#link7374722d7265706c616365), [`str-replace*`](#link7374722d7265706c6163652a), [`instr`](#link696e737472).	 [→index](#idx) [→topic](#str)

## `str-empty?` : procedure/1 {#link7374722d656d7074793f}

Usage: `(str-empty? s) => bool`

Return true if the string `s` is empty, nil otherwise.

See also: [`strlen`](#link7374726c656e).	 [→index](#idx) [→topic](#str)

## `str-exists?` : procedure/2 {#link7374722d6578697374733f}

Usage: `(str-exists? s pred) => bool`

Return true if `pred` returns true for at least one character in string `s`, nil otherwise.

See also: [`exists?`](#link6578697374733f), [`forall?`](#link666f72616c6c3f), [`list-exists?`](#link6c6973742d6578697374733f), [`array-exists?`](#link61727261792d6578697374733f), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#str)

## `str-forall?` : procedure/2 {#link7374722d666f72616c6c3f}

Usage: `(str-forall? s pred) => bool`

Return true if predicate `pred` returns true for all characters in string `s`, nil otherwise.

See also: [`foreach`](#link666f7265616368), [`map`](#link6d6170), [`forall?`](#link666f72616c6c3f), [`array-forall?`](#link61727261792d666f72616c6c3f), [`list-forall`](#link6c6973742d666f72616c6c), [`exists?`](#link6578697374733f).	 [→index](#idx) [→topic](#str)

## `str-foreach` : procedure/2 {#link7374722d666f7265616368}

Usage: `(str-foreach s proc)`

Apply `proc` to each element of string `s` in order, for the side effects.

See also: [`foreach`](#link666f7265616368), [`list-foreach`](#link6c6973742d666f7265616368), [`array-foreach`](#link61727261792d666f7265616368), [`map`](#link6d6170).	 [→index](#idx) [→topic](#str)

## `str-index` : procedure/2 or more {#link7374722d696e646578}

Usage: `(str-index s chars [pos]) => int`

Find the first char in `s` that is in the charset `chars`, starting from the optional `pos` in `s`, and return its index in the string. If no macthing char is found, nil is returned.

See also: [`strsplit`](#link73747273706c6974), [`chars`](#link6368617273), [`inchars`](#link696e6368617273).	 [→index](#idx) [→topic](#str)

## `str-join` : procedure/2 {#link7374722d6a6f696e}

Usage: `(str-join li del) => str`

Join a list of strings `li` where each of the strings is separated by string `del`, and return the result string.

See also: [`strlen`](#link7374726c656e), [`strsplit`](#link73747273706c6974), [`str-slice`](#link7374722d736c696365).	 [→index](#idx) [→topic](#str)

## `str-port?` : procedure/1 {#link7374722d706f72743f}

Usage: `(str-port? p) => bool`

Return true if `p` is a string port, nil otherwise.

See also: [`port?`](#link706f72743f), [`file-port?`](#link66696c652d706f72743f), [`stropen`](#link7374726f70656e), [`open`](#link6f70656e).	 [→index](#idx) [→topic](#fileio)

## `str-ref` : procedure/2 {#link7374722d726566}

Usage: `(str-ref s n) => n`

Return the unicode char as integer at position `n` in `s`. Strings are 0-indexed.

See also: [`nth`](#link6e7468).	 [→index](#idx) [→topic](#str)

## `str-remove-number` : procedure/1 {#link7374722d72656d6f76652d6e756d626572}

Usage: `(str-remove-number s [del]) => str`

Remove the suffix number in `s`, provided there is one and it is separated from the rest of the string by `del`, where the default is a space character. For instance, "Test 29" will be converted to "Test", "User-Name1-23-99" with delimiter "-" will be converted to "User-Name1-23". This function will remove intermediate delimiters in the middle of the string, since it disassembles and reassembles the string, so be aware that this is not preserving inputs in that respect.

See also: [`strsplit`](#link73747273706c6974).	 [→index](#idx) [→topic](#str)

## `str-remove-prefix` : procedure/1 {#link7374722d72656d6f76652d707265666978}

Usage: `(str-remove-prefix s prefix) => str`

Remove the prefix `prefix` from string `s`, return the string without the prefix. If the prefix does not match, `s` is returned. If `prefix` is longer than `s` and matches, the empty string is returned.

See also: [`str-remove-suffix`](#link7374722d72656d6f76652d737566666978).	 [→index](#idx) [→topic](#str)

## `str-remove-suffix` : procedure/1 {#link7374722d72656d6f76652d737566666978}

Usage: `(str-remove-suffix s suffix) => str`

remove the suffix `suffix` from string `s`, return the string without the suffix. If the suffix does not match, `s` is returned. If `suffix` is longer than `s` and matches, the empty string is returned.

See also: [`str-remove-prefix`](#link7374722d72656d6f76652d707265666978).	 [→index](#idx) [→topic](#str)

## `str-replace` : procedure/4 {#link7374722d7265706c616365}

Usage: `(str-replace s t1 t2 n) => str`

Replace the first `n` instances of substring `t1` in `s` by `t2.`

See also: [`str-replace*`](#link7374722d7265706c6163652a), [`str-count-substr`](#link7374722d636f756e742d737562737472).	 [→index](#idx) [→topic](#str)

## `str-replace*` : procedure/3 {#link7374722d7265706c6163652a}

Usage: `(str-replace* s t1 t2) => str`

Replace all non-overlapping substrings `t1` in `s` by `t2.`

See also: [`str-replace`](#link7374722d7265706c616365), [`str-count-substr`](#link7374722d636f756e742d737562737472).	 [→index](#idx) [→topic](#str)

## `str-reverse` : procedure/1 {#link7374722d72657665727365}

Usage: `(str-reverse s) => str`

Reverse string `s.`

See also: [`reverse`](#link72657665727365), [`array-reverse`](#link61727261792d72657665727365), [`list-reverse`](#link6c6973742d72657665727365).	 [→index](#idx) [→topic](#str)

## `str-segment` : procedure/3 {#link7374722d7365676d656e74}

Usage: `(str-segment str start end) => list`

Parse a string `str` into words that start with one of the characters in string `start` and end in one of the characters in string `end` and return a list consisting of lists of the form (bool s) where bool is true if the string starts with a character in `start`, nil otherwise, and `s` is the extracted string including start and end characters.

See also: [`str+`](#link7374722b), [`strsplit`](#link73747273706c6974), [`fmt`](#link666d74), [`strbuild`](#link7374726275696c64).	 [→index](#idx) [→topic](#str)

## `str-slice` : procedure/3 {#link7374722d736c696365}

Usage: `(str-slice s low high) => s`

Return a slice of string `s` starting at character with index `low` (inclusive) and ending at character with index `high` (exclusive).

See also: [`slice`](#link736c696365).	 [→index](#idx) [→topic](#str)

## `str?` : procedure/1 {#link7374723f}

Usage: `(str? s) => bool`

Return true if `s` is a string, nil otherwise.

See also: [`num?`](#link6e756d3f), [`atom?`](#link61746f6d3f), [`sym?`](#link73796d3f), [`closure?`](#link636c6f737572653f), [`intrinsic?`](#link696e7472696e7369633f), [`macro?`](#link6d6163726f3f).	 [→index](#idx)

## `strbuild` : procedure/2 {#link7374726275696c64}

Usage: `(strbuild s n) => str`

Build a string by repeating string `s`` n` times.

See also: [`str+`](#link7374722b).	 [→index](#idx) [→topic](#str)

## `strcase` : procedure/2 {#link73747263617365}

Usage: `(strcase s sel) => str`

Change the case of the string `s` according to selector `sel` and return a copy. Valid values for `sel` are 'lower for conversion to lower-case, 'upper for uppercase, 'title for title case and 'utf-8 for utf-8 normalization (which replaces unprintable characters with "?").

See also: [`strmap`](#link7374726d6170).	 [→index](#idx) [→topic](#str)

## `strcenter` : procedure/2 {#link73747263656e746572}

Usage: `(strcenter s n) => str`

Center string `s` by wrapping space characters around it, such that the total length the result string is `n.`

See also: [`strleft`](#link7374726c656674), [`strright`](#link7374727269676874), [`strlimit`](#link7374726c696d6974).	 [→index](#idx) [→topic](#str)

## `strcnt` : procedure/2 {#link737472636e74}

Usage: `(strcnt s del) => int`

Returnt the number of non-overlapping substrings `del` in `s.`

See also: [`strsplit`](#link73747273706c6974), [`str-index`](#link7374722d696e646578).	 [→index](#idx) [→topic](#str)

## `strleft` : procedure/2 {#link7374726c656674}

Usage: `(strleft s n) => str`

Align string `s` left by adding space characters to the right of it, such that the total length the result string is `n.`

See also: [`strcenter`](#link73747263656e746572), [`strright`](#link7374727269676874), [`strlimit`](#link7374726c696d6974).	 [→index](#idx) [→topic](#str)

## `strlen` : procedure/1 {#link7374726c656e}

Usage: `(strlen s) => int`

Return the length of `s.`

See also: [`len`](#link6c656e), [`seq?`](#link7365713f), [`str?`](#link7374723f).	 [→index](#idx) [→topic](#str)

## `strless` : procedure/2 {#link7374726c657373}

Usage: `(strless s1 s2) => bool`

Return true if string `s1` < `s2` in lexicographic comparison, nil otherwise.

See also: [`sort`](#link736f7274), [`array-sort`](#link61727261792d736f7274), [`strcase`](#link73747263617365).	 [→index](#idx) [→topic](#str)

## `strlimit` : procedure/2 {#link7374726c696d6974}

Usage: `(strlimit s n) => str`

Return a string based on `s` cropped to a maximal length of `n` (or less if `s` is shorter).

See also: [`strcenter`](#link73747263656e746572), [`strleft`](#link7374726c656674), [`strright`](#link7374727269676874).	 [→index](#idx) [→topic](#str)

## `strmap` : procedure/2 {#link7374726d6170}

Usage: `(strmap s proc) => str`

Map function `proc`, which takes a number and returns a number, over all unicode characters in `s` and return the result as new string.

See also: [`map`](#link6d6170).	 [→index](#idx) [→topic](#str)

## `stropen` : procedure/1 {#link7374726f70656e}

Usage: `(stropen s) => streamport`

Open the string `s` as input stream.

See also: [`open`](#link6f70656e), [`close`](#link636c6f7365).	 [→index](#idx) [→topic](#str)

## `strright` : procedure/2 {#link7374727269676874}

Usage: `(strright s n) => str`

Align string `s` right by adding space characters in front of it, such that the total length the result string is `n.`

See also: [`strcenter`](#link73747263656e746572), [`strleft`](#link7374726c656674), [`strlimit`](#link7374726c696d6974).	 [→index](#idx) [→topic](#str)

## `strsplit` : procedure/2 {#link73747273706c6974}

Usage: `(strsplit s del) => array`

Return an array of strings obtained from `s` by splitting `s` at each occurrence of string `del.`

See also: [`str?`](#link7374723f).	 [→index](#idx) [→topic](#str)

## `struct-index` : procedure/1 {#link7374727563742d696e646578}

Usage: `(struct-index s) => dict`

Returns the index of struct `s` as a dict. This dict is an internal representation of the struct's instance data.

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx) [→topic](#oop)

## `struct-instantiate` : procedure/2 {#link7374727563742d696e7374616e7469617465}

Usage: `(struct-instantiate s li) => record`

Instantiates the struct `s` with property a-list `li` as values for its properties and return the record. If a property is not in `li`, its value is set to nil.

See also: [`make`](#link6d616b65), [`defstruct`](#link646566737472756374), [`struct?`](#link7374727563743f), [`record?`](#link7265636f72643f).	 [→index](#idx) [→topic](#oop)

## `struct-name` : procedure/1 {#link7374727563742d6e616d65}

Usage: `(struct-name s) => sym`

Returns the name of a struct `s`. This is rarely needed since the struct is bound to a symbol with the same name.

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx) [→topic](#oop)

## `struct-props` : procedure/1 {#link7374727563742d70726f7073}

Usage: `(struct-props s) => dict`

Returns the properties of structure `s` as dict.

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx) [→topic](#oop)

## `struct-size` : procedure/1 {#link7374727563742d73697a65}

Usage: `(strict-size s) => int`

Returns the number of properties of struct `s.`

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx) [→topic](#oop)

## `struct?` : procedure/1 {#link7374727563743f}

Usage: `(struct? datum) => boo`

Returns true if `datum` is a struct, nil otherwise.

See also: [`defstruct`](#link646566737472756374).	 [→index](#idx) [→topic](#oop)

## `sub1` : procedure/1 {#link73756231}

Usage: `(sub1 n) => num`

Subtract 1 from `n.`

See also: [`add1`](#link61646431), [`+`](#link2b), [`-`](#link2d).	 [→index](#idx) [→topic](#numeric)

## `supers` : procedure/1 {#link737570657273}

Usage: `(supers c) => li`

Return the list of superclasses of class `c`. An error occurs if `c` is not a valid class.

See also: [`class?`](#link636c6173733f), [`isa?`](#link6973613f), [`class-name`](#link636c6173732d6e616d65).	 [→index](#idx) [→topic](#oop)

## `sym->str` : procedure/1 {#link73796d2d3e737472}

Usage: `(sym->str sym) => str`

Convert a symbol into a string.

See also: [`str->sym`](#link7374722d3e73796d), [`intern`](#link696e7465726e), [`make-symbol`](#link6d616b652d73796d626f6c).	 [→index](#idx) [→topic](#conversion)

## `sym?` : procedure/1 {#link73796d3f}

Usage: `(sym? sym) => bool`

Return true if `sym` is a symbol, nil otherwise.

See also: [`str?`](#link7374723f), [`atom?`](#link61746f6d3f).	 [→index](#idx) [→topic](#lisp)

## `synout` : procedure/1 {#link73796e6f7574}

Usage: `(synout arg)`

Like out, but enforcing a new input line afterwards. This needs to be used when outputing concurrently in a future or task.

See also: [`out`](#link6f7574), [`outy`](#link6f757479), [`synouty`](#link73796e6f757479).	 [→index](#idx)

**Warning: Concurrent display output can lead to unexpected visual results and ought to be avoided.** [→topic](#ui)

## `synouty` : procedure/1 {#link73796e6f757479}

Usage: `(synouty li)`

Like outy, but enforcing a new input line afterwards. This needs to be used when outputing concurrently in a future or task.

See also: [`synout`](#link73796e6f7574), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx)

**Warning: Concurrent display output can lead to unexpected visual results and ought to be avoided.**

## `sys-key?` : procedure/1 {#link7379732d6b65793f}

Usage: `(sys-key? key) => bool`

Return true if the given sys key `key` exists, nil otherwise.

See also: [`sys`](#link737973), [`setsys`](#link736574737973).	 [→index](#idx) [→topic](#system)

## `sysmsg` : procedure/1 {#link7379736d7367}

Usage: `(sysmsg msg)`

Asynchronously display a system message string `msg` if in console or page mode, otherwise the message is logged.

See also: [`sysmsg*`](#link7379736d73672a), [`synout`](#link73796e6f7574), [`synouty`](#link73796e6f757479), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx) [→topic](#system)

## `sysmsg*` : procedure/1 {#link7379736d73672a}

Usage: `(sysmsg* msg)`

Display a system message string `msg` if in console or page mode, otherwise the message is logged.

See also: [`sysmsg`](#link7379736d7367), [`synout`](#link73796e6f7574), [`synouty`](#link73796e6f757479), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx) [→topic](#system)

## `take` : procedure/3 {#link74616b65}

Usage: `(take seq n) => seq`

Return the sequence consisting of the `n` first elements of `seq.`

See also: [`list`](#link6c697374), [`array`](#link6172726179), [`string`](#link737472696e67), [`nth`](#link6e7468), [`seq?`](#link7365713f).	 [→index](#idx) [→topic](#seq)

## `task` : procedure/1 {#link7461736b}

Usage: `(task sel proc) => int`

Create a new task for concurrently running `proc`, a procedure that takes its own ID as argument. The `sel` argument must be a symbol in '(auto manual remove). If `sel` is 'remove, then the task is always removed from the task table after it has finished, even if an error has occurred. If sel is 'auto, then the task is removed from the task table if it ends without producing an error. If `sel` is 'manual then the task is not removed from the task table, its state is either 'canceled, 'finished, or 'error, and it and must be removed manually with `task-remove` or `prune-task-table`. Broadcast messages are never removed. Tasks are more heavy-weight than futures and allow for message-passing.

See also: [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e), [`task-state`](#link7461736b2d7374617465), [`task-broadcast`](#link7461736b2d62726f616463617374), [`task-send`](#link7461736b2d73656e64), [`task-recv`](#link7461736b2d72656376), [`task-remove`](#link7461736b2d72656d6f7665), [`prune-task-table`](#link7072756e652d7461736b2d7461626c65).	 [→index](#idx) [→topic](#concurrency)

## `task-broadcast` : procedure/2 {#link7461736b2d62726f616463617374}

Usage: `(task-broadcast id msg)`

Send a message from task `id` to the blackboard. Tasks automatically send the message 'finished to the blackboard when they are finished.

See also: [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e), [`task-state`](#link7461736b2d7374617465), [`task-send`](#link7461736b2d73656e64), [`task-recv`](#link7461736b2d72656376).	 [→index](#idx) [→topic](#concurrency)

## `task-recv` : procedure/1 {#link7461736b2d72656376}

Usage: `(task-recv id) => any`

Receive a message for task `id`, or nil if there is no message. This is typically used by the task with `id` itself to periodically check for new messages while doing other work. By convention, if a task receives the message 'end it ought to terminate at the next convenient occasion, whereas upon receiving 'cancel it ought to terminate in an expedited manner.

See also: [`task-send`](#link7461736b2d73656e64), [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e), [`task-state`](#link7461736b2d7374617465), [`task-broadcast`](#link7461736b2d62726f616463617374).	 [→index](#idx)

**Warning: Busy polling for new messages in a tight loop is inefficient and ought to be avoided.** [→topic](#concurrency)

## `task-remove` : procedure/1 {#link7461736b2d72656d6f7665}

Usage: `(task-remove id)`

Remove task `id` from the task table. The task can no longer be interacted with.

See also: [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-state`](#link7461736b2d7374617465).	 [→index](#idx) [→topic](#concurrency)

## `task-run` : procedure/1 {#link7461736b2d72756e}

Usage: `(task-run id)`

Run task `id`, which must have been previously created with task. Attempting to run a task that is already running results in an error unless `silent?` is true. If silent? is true, the function does never produce an error.

See also: [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-state`](#link7461736b2d7374617465), [`task-send`](#link7461736b2d73656e64), [`task-recv`](#link7461736b2d72656376), [`task-broadcast-`](#link7461736b2d62726f6164636173742d).	 [→index](#idx) [→topic](#concurrency)

## `task-schedule` : procedure/1 {#link7461736b2d7363686564756c65}

Usage: `(task-schedule sel id)`

Schedule task `id` for running, starting it as soon as other tasks have finished. The scheduler attempts to avoid running more than (cpunum) tasks at once.

See also: [`task`](#link7461736b), [`task-run`](#link7461736b2d72756e).	 [→index](#idx) [→topic](#concurrency)

## `task-send` : procedure/2 {#link7461736b2d73656e64}

Usage: `(task-send id msg)`

Send a message `msg` to task `id`. The task needs to cooperatively use task-recv to reply to the message. It is up to the receiving task what to do with the message once it has been received, or how often to check for new messages.

See also: [`task-broadcast`](#link7461736b2d62726f616463617374), [`task-recv`](#link7461736b2d72656376), [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e), [`task-state`](#link7461736b2d7374617465).	 [→index](#idx) [→topic](#concurrency)

## `task-state` : procedure/1 {#link7461736b2d7374617465}

Usage: `(task-state id) => sym`

Return the state of the task, which is a symbol in '(finished error stopped new waiting running).

See also: [`task`](#link7461736b), [`task?`](#link7461736b3f), [`task-run`](#link7461736b2d72756e), [`task-broadcast`](#link7461736b2d62726f616463617374), [`task-recv`](#link7461736b2d72656376), [`task-send`](#link7461736b2d73656e64).	 [→index](#idx) [→topic](#concurrency)

## `task?` : procedure/1 {#link7461736b3f}

Usage: `(task? id) => bool`

Check whether the given `id` is for a valid task, return true if it is valid, nil otherwise.

See also: [`task`](#link7461736b), [`task-run`](#link7461736b2d72756e), [`task-state`](#link7461736b2d7374617465), [`task-broadcast`](#link7461736b2d62726f616463617374), [`task-send`](#link7461736b2d73656e64), [`task-recv`](#link7461736b2d72656376).	 [→index](#idx) [→topic](#concurrency)

## `terpri` : procedure/0 {#link746572707269}

Usage: `(terpri)`

Advance the host OS terminal to the next line.

See also: [`princ`](#link7072696e63), [`out`](#link6f7574), [`outy`](#link6f757479).	 [→index](#idx) [→topic](#console)

## `testing` : macro/1 {#link74657374696e67}

Usage: `(testing name)`

Registers the string `name` as the name of the tests that are next registered with expect.

See also: [`expect`](#link657870656374), [`expect-err`](#link6578706563742d657272), [`expect-ok`](#link6578706563742d6f6b), [`run-selftest`](#link72756e2d73656c6674657374).	 [→index](#idx) [→topic](#system)

## `text-grid-show-line-numbers?` : procedure/1 {#link746578742d677269642d73686f772d6c696e652d6e756d626572733f}

Usage: `(text-grid-show-line-numbers? grid) => bool`

Return true if the text `grid` shows line numbers, nil otherwise.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-whitespace?`](#link746578742d677269642d73686f772d776869746573706163653f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx) [→topic](#gui)

## `text-grid-show-whitespace?` : procedure/1 {#link746578742d677269642d73686f772d776869746573706163653f}

Usage: `(text-grid-show-whitespace? grid) => bool`

Return true if the text `grid` shows whitespace glyphs, nil otherwise.

See also: [`new-text-grid`](#link6e65772d746578742d67726964), [`text-grid-show-line-numbers?`](#link746578742d677269642d73686f772d6c696e652d6e756d626572733f), [`get-text-grid-tab-width`](#link6765742d746578742d677269642d7461622d7769647468), [`set-text-grid-tab-width`](#link7365742d746578742d677269642d7461622d7769647468), [`set-text-grid-show-line-numbers`](#link7365742d746578742d677269642d73686f772d6c696e652d6e756d62657273), [`set-text-grid-show-whitespace`](#link7365742d746578742d677269642d73686f772d77686974657370616365), [`get-text-grid-row`](#link6765742d746578742d677269642d726f77), [`get-text-grid-row-text`](#link6765742d746578742d677269642d726f772d74657874), [`set-text-grid-cell`](#link7365742d746578742d677269642d63656c6c), [`get-text-grid-cell`](#link6765742d746578742d677269642d63656c6c), [`set-text-grid-row`](#link7365742d746578742d677269642d726f77), [`set-text-grid-row-style`](#link7365742d746578742d677269642d726f772d7374796c65), [`set-text-grid-rune`](#link7365742d746578742d677269642d72756e65), [`set-text-grid-style`](#link7365742d746578742d677269642d7374796c65), [`set-text-grid-style-range`](#link7365742d746578742d677269642d7374796c652d72616e6765), [`set-text-grid-text`](#link7365742d746578742d677269642d74657874), [`get-text-grid-text`](#link6765742d746578742d677269642d74657874).	 [→index](#idx) [→topic](#gui)

## `the-color` : procedure/1 {#link7468652d636f6c6f72}

Usage: `(the-color colors-spec) => (r g b a)`

Return the color list (r g b a) based on a color specification, which may be a color list (r g b), a color selector for (color selector) or a color name such as 'dark-blue.

See also: [`*colors*`](#link2a636f6c6f72732a), [`color`](#link636f6c6f72), [`set-color`](#link7365742d636f6c6f72), [`outy`](#link6f757479).	 [→index](#idx) [→topic](#ui)

## `the-color-names` : procedure/0 {#link7468652d636f6c6f722d6e616d6573}

Usage: `(the-color-names) => li`

Return the list of color names in *colors*.

See also: [`*colors*`](#link2a636f6c6f72732a), [`the-color`](#link7468652d636f6c6f72).	 [→index](#idx) [→topic](#ui)

## `theme-color` : procedure/1 {#link7468656d652d636f6c6f72}

Usage: `(theme-color selector) => li`

Obtain a theme color as color list. `selector` must be one of '(foreground background button disabled-button disabled disabled-text error focus hover input-background input-border menu-background overlay-background place-holder pressed primary scroll-bar selection separator shadow success warning).

See also: [`theme-icon`](#link7468656d652d69636f6e), [`nrgba64`](#link6e726762613634), [`nrgba`](#link6e72676261), [`color->color-64`](#link636f6c6f722d3e636f6c6f722d3634), [`color-64->color`](#link636f6c6f722d36342d3e636f6c6f72), [`*colors*`](#link2a636f6c6f72732a).	 [→index](#idx) [→topic](#gui)

## `theme-icon` : procedure/1 {#link7468656d652d69636f6e}

Usage: `(theme-icon selector) => int`

Obtain a pre-defined icon from the application's theme based on the symbol `selector`, which may be one of '(cancel check-button check-button-checked color-achromatic color-chromatic color-palette computer confirm content-add content-clear content-copy content-cut content-paste content-redo content-remove content-undo delete document-create document-print document download error file-application file-audio file-image file-text file-video file folder-new folder-open folder grid help history home info list login logout mail-attachment mail-compose mail-forward mail-reply-all mail-reply mail-send media-fast-forward media-fast-rewind media-music media-pause media-photo media-play media-record media-replay media-skip-next media-skip-previous media-stop media-video media-expand menu more-horizontal more-vertical move-down move-up navigate-back navigate-next question radio-button radio-button-checked search-replace search settings storage upload view-full-screen view-refresh view-restore visibility-off visibility volume-down volume-mute volume-up warning).

See also: [`new-icon`](#link6e65772d69636f6e), [`new-image-from-`](#link6e65772d696d6167652d66726f6d2d), [`new-image-from-resource`](#link6e65772d696d6167652d66726f6d2d7265736f75726365).	 [→index](#idx) [→topic](#gui)

## `theme-is-dark?` : procedure/1 {#link7468656d652d69732d6461726b3f}

Usage: `(theme-is-dark?) => bool`

Return true if the current GUI theme is dark, nil otherwise.

See also: [`theme-color`](#link7468656d652d636f6c6f72), [`*colors*`](#link2a636f6c6f72732a).	 [→index](#idx) [→topic](#gui)

## `time` : procedure/1 {#link74696d65}

Usage: `(time proc) => int`

Return the time in nanoseconds that it takes to execute the procedure with no arguments `proc.`

See also: [`now-ns`](#link6e6f772d6e73), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `trigger-menu-last` : procedure/1 {#link747269676765722d6d656e752d6c617374}

Usage: `(trigger-menu-last menu)`

Find the last active menu or submenu item and trigger it.

See also: [`activate-menu-last-submenu`](#link61637469766174652d6d656e752d6c6173742d7375626d656e75), [`activate-menu-next`](#link61637469766174652d6d656e752d6e657874), [`activate-menu-previous`](#link61637469766174652d6d656e752d70726576696f7573), [`new-menu`](#link6e65772d6d656e75).	 [→index](#idx) [→topic](#gui)

## `truncate` : procedure/1 or more {#link7472756e63617465}

Usage: `(truncate x [y]) => int`

Round down to nearest integer of `x`. If `y` is present, divide `x` by `y` and round down to the nearest integer.

See also: [`div`](#link646976), [`/`](#link2f), [`int`](#link696e74).	 [→index](#idx) [→topic](#numeric)

## `try` : macro/2 or more {#link747279}

Usage: `(try (finals ...) body ...)`

Evaluate the forms of the `body` and afterwards the forms in `finals`. If during the execution of `body` an error occurs, first all `finals` are executed and then the error is printed by the default error printer.

See also: [`with-final`](#link776974682d66696e616c), [`with-error-handler`](#link776974682d6572726f722d68616e646c6572).	 [→index](#idx) [→topic](#system)

## `type-of` : macro/1 {#link747970652d6f66}

Usage: `(type-of datum) => sym`

Returns the type of `datum` as symbol like type-of* but without having to quote the argument. If `datum` is an unbound symbol, then this macro returns 'unbound. Otherwise the type of a given symbol's value or the type of a given literal is returned.

See also: [`type-of*`](#link747970652d6f662a).	 [→index](#idx) [→topic](#lisp)

## `type-of*` : procedure/1 {#link747970652d6f662a}

Usage: `(type-of* datum) => sym`

Return the type of `datum` as a symbol. This uses existing predicates and therefore is not faster than testing with predicates directly.

See also: [`num?`](#link6e756d3f), [`str?`](#link7374723f), [`sym?`](#link73796d3f), [`list?`](#link6c6973743f), [`array?`](#link61727261793f), [`bool?`](#link626f6f6c3f), [`eof?`](#link656f663f), [`boxed?`](#link626f7865643f), [`intrinsic?`](#link696e7472696e7369633f), [`closure?`](#link636c6f737572653f), [`macro?`](#link6d6163726f3f), [`blob?`](#link626c6f623f).	 [→index](#idx) [→topic](#lisp)

## `unfocus-canvas-objects` : procedure/1 {#link756e666f6375732d63616e7661732d6f626a65637473}

Usage: `(unfocus-canvas-objects canvas)`

Remove the focus on any user interface element in `canvas.`

See also: [`get-window-canvas`](#link6765742d77696e646f772d63616e766173), [`focus-canvas-object`](#link666f6375732d63616e7661732d6f626a656374), [`focus-next-canvas-object`](#link666f6375732d6e6578742d63616e7661732d6f626a656374), [`focus-previous-canvas-object`](#link666f6375732d70726576696f75732d63616e7661732d6f626a656374), [`get-focused-canvas-object`](#link6765742d666f63757365642d63616e7661732d6f626a656374).	 [→index](#idx) [→topic](#gui)

## `unicode.is-control?` : procedure/1 {#link756e69636f64652e69732d636f6e74726f6c3f}

Usage: `(unicode.is-control? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a control character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-digit?` : procedure/1 {#link756e69636f64652e69732d64696769743f}

Usage: `(unicode.is-digit? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a numerical digit, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-graphic?` : procedure/1 {#link756e69636f64652e69732d677261706869633f}

Usage: `(unicode.is-graphic? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` is graphically visible, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-letter?` : procedure/1 {#link756e69636f64652e69732d6c65747465723f}

Usage: `(unicode.is-letter? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a letter, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-lower?` : procedure/1 {#link756e69636f64652e69732d6c6f7765723f}

Usage: `(unicode.is-lower? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` is a lowercase character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-mark?` : procedure/1 {#link756e69636f64652e69732d6d61726b3f}

Usage: `(unicode.is-mark? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a mark (unicode category M), nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-number?` : procedure/1 {#link756e69636f64652e69732d6e756d6265723f}

Usage: `(unicode.is-number? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a letter that may occur in a number, nil otherwise. This is for testing the unicode number category, which contains number and digit glyphs in other writing system. Note that it does not test whether a character belongs to a valid Z3S5 Lisp number.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-print?` : procedure/1 {#link756e69636f64652e69732d7072696e743f}

Usage: `(unicode.is-print? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a printable character.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-punct?` : procedure/1 {#link756e69636f64652e69732d70756e63743f}

Usage: `(unicode.is-punct? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a punctuation character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-space?` : procedure/1 {#link756e69636f64652e69732d73706163653f}

Usage: `(unicode.is-space? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a white space character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-symbol?` : procedure/1 {#link756e69636f64652e69732d73796d626f6c3f}

Usage: `(unicode.is-symbol? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a symbol, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-title?` : procedure/1 {#link756e69636f64652e69732d7469746c653f}

Usage: `(unicode.is-title? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents a titlecase character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-upper?`](#link756e69636f64652e69732d75707065723f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unicode.is-upper?` : procedure/1 {#link756e69636f64652e69732d75707065723f}

Usage: `(unicode.is-upper? s) => bool`

Return true if number `s` or the first unicode glyph of string `s` represents an uppercase character, nil otherwise.

See also: [`char->str`](#link636861722d3e737472), [`str->chars`](#link7374722d3e6368617273), [`unicode.is-space?`](#link756e69636f64652e69732d73706163653f), [`unicode.is-punct?`](#link756e69636f64652e69732d70756e63743f), [`unicode.is-symbol?`](#link756e69636f64652e69732d73796d626f6c3f), [`unicode.is-title?`](#link756e69636f64652e69732d7469746c653f), [`unicode.is-control?`](#link756e69636f64652e69732d636f6e74726f6c3f), [`unicode.is-digit?`](#link756e69636f64652e69732d64696769743f), [`unicode.is-graphic?`](#link756e69636f64652e69732d677261706869633f), [`unicode.is-letter?`](#link756e69636f64652e69732d6c65747465723f), [`unicode.is-lower?`](#link756e69636f64652e69732d6c6f7765723f), [`unicode.is-mark?`](#link756e69636f64652e69732d6d61726b3f), [`unicode.is-number?`](#link756e69636f64652e69732d6e756d6265723f), [`unicode.is-print?`](#link756e69636f64652e69732d7072696e743f).	 [→index](#idx) [→topic](#unicode)

## `unless` : macro/1 or more {#link756e6c657373}

Usage: `(unless cond expr ...) => any`

Evaluate expressions `expr` if `cond` is not true, returns void otherwise.

See also: [`if`](#link6966), [`when`](#link7768656e), [`cond`](#link636f6e64).	 [→index](#idx) [→topic](#lisp)

## `unprotect` : procedure/0 or more {#link756e70726f74656374}

Usage: `(unprotect [sym] ...)`

Unprotect symbols `sym` ..., allowing mutation or rebinding them. The symbols need to be quoted. This operation requires the permission 'allow-unprotect to be set, or else an error is caused.

See also: [`protect`](#link70726f74656374), [`protected?`](#link70726f7465637465643f), [`dict-unprotect`](#link646963742d756e70726f74656374), [`dict-protected?`](#link646963742d70726f7465637465643f), [`permissions`](#link7065726d697373696f6e73), [`permission?`](#link7065726d697373696f6e3f), [`setq`](#link73657471), [`bind`](#link62696e64), [`interpret`](#link696e74657270726574).	 [→index](#idx) [→topic](#system)

## `unprotect-toplevel-symbols` : procedure/0 {#link756e70726f746563742d746f706c6576656c2d73796d626f6c73}

Usage: `(unprotect-toplevel-symbols)`

Attempts to unprotect all toplevel symbols.

See also: [`protect-toplevel-symbols`](#link70726f746563742d746f706c6576656c2d73796d626f6c73), [`protect`](#link70726f74656374), [`unprotect`](#link756e70726f74656374), [`declare-unprotected`](#link6465636c6172652d756e70726f746563746564).	 [→index](#idx) [→topic](#system)

## `until` : macro/1 or more {#link756e74696c}

Usage: `(until test body ...)`

Evaluate the expressions in `body` until `test` is nil. Unlike a while loop, until executes the body expressions at least once.

See also: [`while`](#link7768696c65), [`letrec`](#link6c6574726563), [`dotimes`](#link646f74696d6573), [`dolist`](#link646f6c697374).	 [→index](#idx) [→topic](#lisp)

## `uri?` : procedure/1 {#link7572693f}

Usage: `(uri? s) => bool`

Return true if string `s` represents a valid URI, nil otherwise.

See also: [`list-uri`](#link6c6973742d757269), [`listable-uri?`](#link6c69737461626c652d7572693f).	 [→index](#idx) [→topic](#gui)

## `valid?` : procedure/1 {#link76616c69643f}

Usage: `(valid? obj) => bool`

Return true if `obj` is a valid object, nil otherwise. What exactly object validity means is undefined, but certain kind of objects such as graphics objects may be marked invalid when they can no longer be used because they have been disposed off by a subsystem and cannot be automatically garbage collected. Generally, invalid objects ought no longer be used and need to be discarded.

See also: [`blob?`](#link626c6f623f).	 [→index](#idx) [→topic](#boxed)

## `validate-object` : procedure/2 {#link76616c69646174652d6f626a656374}

Usage: `(validate-object obj) => str`

Validate the validatable object `obj` programmatically and return the validation failure as string, or the empty string if validation succeeded. It sometimes makes sense to call this explicitly in order to force the object to display its validation state.

See also: [`set-object-on-validation-change-callback`](#link7365742d6f626a6563742d6f6e2d76616c69646174696f6e2d6368616e67652d63616c6c6261636b), [`new-validator`](#link6e65772d76616c696461746f72), [`set-entry-validator`](#link7365742d656e7472792d76616c696461746f72).	 [→index](#idx) [→topic](#gui)

## `void` : procedure/0 or more {#link766f6964}

Usage: `(void [any] ...)`

Always returns void, no matter what values are given to it. Void is a special value that is not printed in the console.

See also: [`void?`](#link766f69643f).	 [→index](#idx) [→topic](#lisp)

## `void?` : procedure/1 {#link766f69643f}

Usage: `(void? datum)`

Return true if `datum` is the special symbol void, nil otherwise.

See also: [`void`](#link766f6964).	 [→index](#idx) [→topic](#lisp)

## `wait-for` : procedure/2 {#link776169742d666f72}

Usage: `(wait-for dict key)`

Block execution until the value for `key` in `dict` is not-nil. This function may wait indefinitely if no other thread sets the value for `key` to not-nil.

See also: [`wait-for*`](#link776169742d666f722a), [`future`](#link667574757265), [`force`](#link666f726365), [`wait-until`](#link776169742d756e74696c), [`wait-until*`](#link776169742d756e74696c2a).	 [→index](#idx)

**Warning: This cannot be used for synchronization of multiple tasks due to potential race-conditions.** [→topic](#concurrency)

## `wait-for*` : procedure/3 {#link776169742d666f722a}

Usage: `(wait-for* dict key timeout)`

Blocks execution until the value for `key` in `dict` is not-nil or `timeout` nanoseconds have passed, and returns that value or nil if waiting timed out. If `timeout` is negative, then the function waits potentially indefinitely without any timeout. If a non-nil key is not found, the function sleeps at least *sync-wait-lower-bound* nanoseconds and up to *sync-wait-upper-bound* nanoseconds until it looks for the key again.

See also: [`future`](#link667574757265), [`force`](#link666f726365), [`wait-for`](#link776169742d666f72), [`wait-until`](#link776169742d756e74696c), [`wait-until*`](#link776169742d756e74696c2a).	 [→index](#idx)

**Warning: This cannot be used for synchronization of multiple tasks due to potential race-conditions.** [→topic](#concurrency)

## `wait-for-empty*` : procedure/3 {#link776169742d666f722d656d7074792a}

Usage: `(wait-for-empty* dict key timeout)`

Blocks execution until the `key` is no longer present in `dict` or `timeout` nanoseconds have passed. If `timeout` is negative, then the function waits potentially indefinitely without any timeout.

See also: [`future`](#link667574757265), [`force`](#link666f726365), [`wait-for`](#link776169742d666f72), [`wait-until`](#link776169742d756e74696c), [`wait-until*`](#link776169742d756e74696c2a).	 [→index](#idx)

**Warning: This cannot be used for synchronization of multiple tasks due to potential race-conditions.** [→topic](#concurrency)

## `wait-until` : procedure/3 {#link776169742d756e74696c}

Usage: `(wait-until dict key pred)`

Blocks execution until the unary predicate `pred` returns true for the value at `key` in `dict`. This function may wait indefinitely if no other thread sets the value in such a way that `pred` returns true when applied to it.

See also: [`wait-for`](#link776169742d666f72), [`future`](#link667574757265), [`force`](#link666f726365), [`wait-until*`](#link776169742d756e74696c2a).	 [→index](#idx)

**Warning: This cannot be used for synchronization of multiple tasks due to potential race-conditions.** [→topic](#concurrency)

## `wait-until*` : procedure/4 {#link776169742d756e74696c2a}

Usage: `(wait-until* dict key pred timeout)`

Blocks execution until the unary predicate `pred` returns true for the value at `key` in `dict`, or `timeout` nanoseconds have passed, and returns the value or nil if waiting timed out. If `timeout` is negative, then the function waits potentially indefinitely without any timeout. If a non-nil key is not found, the function sleeps at least *sync-wait-lower-bound* nanoseconds and up to *sync-wait-upper-bound* nanoseconds until it looks for the key again.

See also: [`future`](#link667574757265), [`force`](#link666f726365), [`wait-for`](#link776169742d666f72), [`wait-until*`](#link776169742d756e74696c2a), [`wait-until`](#link776169742d756e74696c).	 [→index](#idx)

**Warning: This cannot be used for synchronization of multiple tasks due to potential race-conditions.** [→topic](#concurrency)

## `warn` : procedure/1 or more {#link7761726e}

Usage: `(warn msg [args...])`

Output the warning message `msg` in error colors. The optional `args` are applied to the message as in fmt. The message should not end with a newline.

See also: [`error`](#link6572726f72).	 [→index](#idx) [→topic](#system)

## `week+` : procedure/2 {#link7765656b2b}

Usage: `(week+ dateli n) => dateli`

Adds `n` weeks to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`minute+`](#link6d696e7574652b), [`hour+`](#link686f75722b), [`day+`](#link6461792b), [`month+`](#link6d6f6e74682b), [`year+`](#link796561722b), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `week-of-date` : procedure/3 {#link7765656b2d6f662d64617465}

Usage: `(week-of-date Y M D) => int`

Return the week of the date in the year given by year `Y`, month `M`, and day `D.`

See also: [`day-of-week`](#link6461792d6f662d7765656b), [`datestr->datelist`](#link646174657374722d3e646174656c697374), [`date->epoch-ns`](#link646174652d3e65706f63682d6e73), [`epoch-ns->datelist`](#link65706f63682d6e732d3e646174656c697374), [`datestr`](#link64617465737472), [`datestr*`](#link646174657374722a), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `when` : macro/1 or more {#link7768656e}

Usage: `(when cond expr ...) => any`

Evaluate the expressions `expr` if `cond` is true, returns void otherwise.

See also: [`if`](#link6966), [`cond`](#link636f6e64), [`unless`](#link756e6c657373).	 [→index](#idx) [→topic](#lisp)

## `when-permission` : macro/1 or more {#link7768656e2d7065726d697373696f6e}

Usage: `(when-permission perm body ...) => any`

Execute the expressions in `body` if and only if the symbolic permission `perm` is available.

See also: [`permission?`](#link7065726d697373696f6e3f).	 [→index](#idx) [→topic](#system)

## `while` : macro/1 or more {#link7768696c65}

Usage: `(while test body ...)`

Evaluate the expressions in `body` while `test` is not nil.

See also: [`until`](#link756e74696c), [`letrec`](#link6c6574726563), [`dotimes`](#link646f74696d6573), [`dolist`](#link646f6c697374).	 [→index](#idx) [→topic](#lisp)

## `window-fixed-size?` : procedure/1 {#link77696e646f772d66697865642d73697a653f}

Usage: `(window-fixed-size? window) => bool`

Return nil if `window` is fixed size, true otherwise.

See also: [`set-window-fixed-size`](#link7365742d77696e646f772d66697865642d73697a65).	 [→index](#idx) [→topic](#gui)

## `window-full-screen?` : procedure/1 {#link77696e646f772d66756c6c2d73637265656e3f}

Usage: `(window-full-screen? window) => bool`

Return nil if `window` is full screen, true otherwise.

See also: [`set-window-full-screen`](#link7365742d77696e646f772d66756c6c2d73637265656e), [`center-window-on-screen`](#link63656e7465722d77696e646f772d6f6e2d73637265656e).	 [→index](#idx) [→topic](#gui)

## `window-padded?` : procedure/1 {#link77696e646f772d7061646465643f}

Usage: `(window-padded? window) => bool`

Return nil if `window` is padded, true otherwise.

See also: [`set-window-padded`](#link7365742d77696e646f772d706164646564), [`set-window-full-screen`](#link7365742d77696e646f772d66756c6c2d73637265656e), [`center-window-on-screen`](#link63656e7465722d77696e646f772d6f6e2d73637265656e).	 [→index](#idx) [→topic](#gui)

## `with-colors` : procedure/3 {#link776974682d636f6c6f7273}

Usage: `(with-colors textcolor backcolor proc)`

Execute `proc` for display side effects, where the default colors are set to `textcolor` and `backcolor`. These are color specifications like in the-color. After `proc` has finished or if an error occurs, the default colors are restored to their original state.

See also: [`the-color`](#link7468652d636f6c6f72), [`color`](#link636f6c6f72), [`set-color`](#link7365742d636f6c6f72), [`with-final`](#link776974682d66696e616c).	 [→index](#idx) [→topic](#system)

## `with-error-handler` : macro/2 or more {#link776974682d6572726f722d68616e646c6572}

Usage: `(with-error-handler handler body ...)`

Evaluate the forms of the `body` with error handler `handler` in place. The handler is a procedure that takes the error as argument and handles it. If an error occurs in `handler`, a default error handler is used. Handlers are only active within the same thread.

See also: [`with-final`](#link776974682d66696e616c).	 [→index](#idx) [→topic](#system)

## `with-final` : macro/2 or more {#link776974682d66696e616c}

Usage: `(with-final finalizer body ...)`

Evaluate the forms of the `body` with the given finalizer as error handler. If an error occurs, then `finalizer` is called with that error and nil. If no error occurs, `finalizer` is called with nil as first argument and the result of evaluating all forms of `body` as second argument.

See also: [`with-error-handler`](#link776974682d6572726f722d68616e646c6572).	 [→index](#idx) [→topic](#system)

## `with-mutex-lock` : macro/1 or more {#link776974682d6d757465782d6c6f636b}

Usage: `(with-mutex-lock m ...) => any`

Execute the body with mutex `m` locked for writing and unlock the mutex afterwards.

See also: [`with-mutex-rlock`](#link776974682d6d757465782d726c6f636b), [`make-mutex`](#link6d616b652d6d75746578), [`mutex-lock`](#link6d757465782d6c6f636b), [`mutex-rlock`](#link6d757465782d726c6f636b), [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`mutex-runlock`](#link6d757465782d72756e6c6f636b).	 [→index](#idx)

**Warning: Make sure to never lock the same mutex twice from the same task, otherwise a deadlock will occur!**

## `with-mutex-rlock` : macro/1 or more {#link776974682d6d757465782d726c6f636b}

Usage: `(with-mutex-rlock m ...) => any`

Execute the body with mutex `m` locked for reading and unlock the mutex afterwards.

See also: [`with-mutex-lock`](#link776974682d6d757465782d6c6f636b), [`make-mutex`](#link6d616b652d6d75746578), [`mutex-lock`](#link6d757465782d6c6f636b), [`mutex-rlock`](#link6d757465782d726c6f636b), [`mutex-unlock`](#link6d757465782d756e6c6f636b), [`mutex-runlock`](#link6d757465782d72756e6c6f636b).	 [→index](#idx) [→topic](#concurrency)

## `wrap-delete-text-grid` : procedure/8 {#link777261702d64656c6574652d746578742d67726964}

Usage: `(wrap-delete-text-grid grid range-list wrapcol soft-wrap? hard-lf-rune soft-lf-rune cursor-row cursor-column) => li`

This helper implements deletion with word wrapping in `grid`. The `range-list` must contain integers of the form (start-row start-colum end-row end-column), which must be within the grid's maximum row and column ranges. `wrapcol` is an integer indicating the number of chars per line; any more chars are wrapped. If `soft-wrap?` is not nil, then the paragraphs in which deletion takes place are soft-wrapped. `hard-lf-rune` is a string containing the rune for a hard line feed, whereas `soft-lf-rune` is a string containing the rune for soft line feeds. The current `cursor-row` and `cursor-column` must be provided as well; when the function wraps the deleted paragraphs, their values are updated and returned in the list `li`, which is of the form (new-cursor-row new-cursor-column).

See also: [`wrap-insert-text-grid`](#link777261702d696e736572742d746578742d67726964), [`new-text-grid`](#link6e65772d746578742d67726964).	 [→index](#idx) [→topic](#gui)

## `wrap-insert-text-grid` : procedure/8 {#link777261702d696e736572742d746578742d67726964}

Usage: `(wrap-insert-text-grid grid cells row col wrapcol soft-wrap? hard-lf-rune soft-lf-rune) => li`

This helper implements inserting styled text with word wrapping in `grid`. `cells` must be a list of text grid cells, each of which consists of a rune string, and a list containing a foreground and background color, or nil. `row` and `col` are the line and column in `grid` before which the text is inserted. The number of characters per line is indicated with `wrapcol`. If `soft-wrap?` is true, then the paragraph into which it is inserted is soft-word-wrapped, using soft-lf-rune as a line ending. Otherwise, `hard-lf-rune` is used for line-endings, which is also used for the last line of a paragraph. The returned list of the form (new-cursor-row new-cursor-column) reflects the updated cursor position if `row` and `col` are the current cursor position.

See also: [`wrap-delete-text-grid`](#link777261702d64656c6574652d746578742d67726964), [`new-text-grid`](#link6e65772d746578742d67726964).	 [→index](#idx) [→topic](#gui)

## `write` : procedure/2 {#link7772697465}

Usage: `(write p datum) => int`

Write `datum` to output port `p` and return the number of bytes written.

See also: [`write-binary`](#link77726974652d62696e617279), [`write-binary-at`](#link77726974652d62696e6172792d6174), [`read`](#link72656164), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx) [→topic](#fileio)

## `write-binary` : procedure/4 {#link77726974652d62696e617279}

Usage: `(write-binary p buff n offset) => int`

Write `n` bytes starting at `offset` in binary blob `buff` to the stream port `p`. This function returns the number of bytes actually written.

See also: [`write-binary-at`](#link77726974652d62696e6172792d6174), [`read-binary`](#link726561642d62696e617279), [`write`](#link7772697465), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx) [→topic](#fileio)

## `write-binary-at` : procedure/5 {#link77726974652d62696e6172792d6174}

Usage: `(write-binary-at p buff n offset fpos) => int`

Write `n` bytes starting at `offset` in binary blob `buff` to the seekable stream port `p` at the stream position `fpos`. If there is not enough data in `p` to overwrite at position `fpos`, then an error is caused and only part of the data might be written. The function returns the number of bytes actually written.

See also: [`read-binary`](#link726561642d62696e617279), [`write-binary`](#link77726974652d62696e617279), [`write`](#link7772697465), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx) [→topic](#fileio)

## `write-string` : procedure/2 {#link77726974652d737472696e67}

Usage: `(write-string p s) => int`

Write string `s` to output port `p` and return the number of bytes written. LF are *not* automatically converted to CR LF sequences on windows.

See also: [`write`](#link7772697465), [`write-binary`](#link77726974652d62696e617279), [`write-binary-at`](#link77726974652d62696e6172792d6174), [`read`](#link72656164), [`close`](#link636c6f7365), [`open`](#link6f70656e).	 [→index](#idx) [→topic](#fileio)

## `write-zimage` : procedure/4 {#link77726974652d7a696d616765}

Usage: `(write-zimage out min-version info entry-point) => list`

Write the current state of the system as an zimage to stream `out`. The `min-version` argument designates the minimum system version required to load the zimage. The `info` argument should be a list whose first argument is a human-readable string explaining the purpose of the zimage and remainder is user data. The `entry-point` is either nil or an expression that can be evaluated to start the zimage after it has been loaded with run-zimage. The procedure returns a header with information of the zimage.

See also: [`save-zimage`](#link736176652d7a696d616765), [`read-zimage`](#link726561642d7a696d616765), [`load-zimage`](#link6c6f61642d7a696d616765), [`current-zimage`](#link63757272656e742d7a696d616765), [`externalize`](#link65787465726e616c697a65).	 [→index](#idx) [→topic](#zimage)

## `year+` : procedure/2 {#link796561722b}

Usage: `(month+ dateli n) => dateli`

Adds `n` years to the given date `dateli` in datelist format and returns the new datelist.

See also: [`sec+`](#link7365632b), [`minute+`](#link6d696e7574652b), [`hour+`](#link686f75722b), [`day+`](#link6461792b), [`week+`](#link7765656b2b), [`month+`](#link6d6f6e74682b), [`now`](#link6e6f77).	 [→index](#idx) [→topic](#time)

## `zedit-current-selection-text` : procedure/1 {#link7a656469742d63757272656e742d73656c656374696f6e2d74657874}

Usage: `(zedit-current-selection-text editor) => str`

Obtain the currently selected text in `editor`, the empty string if no text is selected.

See also: [`get-zedit-text`](#link6765742d7a656469742d74657874), [`get-zedit-text-range`](#link6765742d7a656469742d746578742d72616e6765), [`get-zedit-selection`](#link6765742d7a656469742d73656c656374696f6e).	 [→index](#idx) [→topic](#gui)

## `zimage-header` : procedure/1 {#link7a696d6167652d686561646572}

Usage: `(zimage-header fi) => li`

Return the zimage header from file `fi.`

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`run-zimage`](#link72756e2d7a696d616765).	 [→index](#idx) [→topic](#zimage)

## `zimage-loadable?` : procedure/1 or more {#link7a696d6167652d6c6f616461626c653f}

Usage: `(zimage-loadable? fi)`

Checks whether the file `fi` is loadable. This does not check whether the file actually is an zimage file, so you can only use this on readable lisp files.

See also: [`zimage-runable?`](#link7a696d6167652d72756e61626c653f), [`load-zimage`](#link6c6f61642d7a696d616765), [`save-zimage`](#link736176652d7a696d616765), [`current-zimage`](#link63757272656e742d7a696d616765).	 [→index](#idx) [→topic](#zimage)

## `zimage-runable?` : procedure/1 or more {#link7a696d6167652d72756e61626c653f}

Usage: `(zimage-runable? [sel] fi`

Returns the non-nil entry-point of the zimage if the the zimage in file `fi` can be run, nil otherwise.

See also: [`load-zimage`](#link6c6f61642d7a696d616765), [`zimage-loadable?`](#link7a696d6167652d6c6f616461626c653f), [`save-zimage`](#link736176652d7a696d616765), [`current-zimage`](#link63757272656e742d7a696d616765).	 [→index](#idx) [→topic](#zimage)

