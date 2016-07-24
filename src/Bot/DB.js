/* eslint-disable xo/filename-case */
'use strict';

const rethink = require('rethinkdbdash');
const APP_CONFIG = require('../../appconfig').APP_CONFIG;

const r = rethink(APP_CONFIG.RETHINKDB);

exports._disruptionChanges = function (eb) {
    return function (cb) {
        return function () {
            r.table('disruptions')
                .filter(r.row('level').ge(1))
                .changes()
                .run(function (err, cursor) {
                    if (err) {
                        eb(err)();
                        return;
                    }

                    cursor.each(function (err, row) {
                        if (err) {
                            eb(err)();
                            return;
                        }

                        if (row !== null && row.new_val !== null) {
                            cb(row.new_val)();
                        }
                    });
                });
        };
    };
};

exports._findRouteByName = function (name) {
    return function (eb) {
        return function (cb) {
            return function () {
                return r.table('routes_info')
                        .getAll(name, {index: 'name'})
                        .coerceTo('array')
                        .run(function (err, res) {
                            if (err) {
                                eb(err)();
                                return;
                            }

                            cb(res)();
                        });
            };
        };
    };
};

/* Recipients For Disruption #rockbandnames */
exports._findRecipientsForDisruption = function (lineName) {
    return function (eb) {
        return function (cb) {
            return function () {
                r.table('messenger_subscriptions')
                    .filter(r.row('status').eq('subscribed').and(r.row('line').eq(lineName)))
                    .run(function (err, cursor) {
                        if (err) {
                            eb(err)();
                            return;
                        }

                        cursor.forEach(function (i) {
                            cb(i)();
                        });
                    });
            };
        };
    };
};
