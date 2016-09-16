/* eslint-disable xo/filename-case */
'use strict';

const rethink = require('rethinkdbdash');
const APP_CONFIG = require('../../appconfig').APP_CONFIG;

const r = rethink(APP_CONFIG.RETHINKDB);

exports._disruptionChanges = function (eb) {
    return function (cb) {
        return function () {
            r.table('disruptions')
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
                            cb(row)();
                        }
                    });
                });
        };
    };
};

exports._disruptionResolvedChanges = function (eb) {
    return function (cb) {
        return function () {
            r.table('disruptions')
                .filter(r.row('level').eq(0))
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

exports._subscribeUserToRoute = function (recipient) {
    return function (routeName) {
        return function (eb) {
            return function (cb) {
                return function () {
                    console.log('SUBSCRIBING', recipient, routeName);
                    r.table('messenger_subscriptions')
                        .insert({route: routeName, recipients: [recipient.id]},
                            {conflict: function (id, oldDoc, newDoc) {
                                return oldDoc.merge({
                                    recipients: oldDoc('recipients').setUnion(newDoc('recipients'))
                                });
                            }})
                        .run(function (err) {
                            if (err) {
                                eb(err)();
                            } else {
                                cb()();
                            }
                        });
                };
            };
        };
    };
};

exports._unsubscribeUserFromRoute = function (recipient) {
    return function (routeName) {
        return function (eb) {
            return function (cb) {
                return function () {
                    console.log('UNSUBSCRIBING', recipient, routeName);
                    r.table('messenger_subscriptions')
                        .get(routeName)
                        .update({
                            recipients: r.row('recipients').setDifference([recipient.id])
                        })
                        .run(function (err) {
                            if (err) {
                                eb(err)();
                            } else {
                                cb()();
                            }
                        });
                };
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
                    .get(lineName).pluck(['recipients']).default([])('recipients')
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

exports._allRoutes = function (eb) {
    return function (cb) {
        return function () {
            r.table('routes_info')
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
