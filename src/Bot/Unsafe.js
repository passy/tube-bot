/* eslint-disable xo/filename-case */
'use strict';

exports.unsafeTraceId = function (s) {
    console.log(JSON.stringify(s, null, 2));
    return s;
};
