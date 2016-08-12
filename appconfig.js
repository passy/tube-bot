'use strict';
const config = require('config');

const getConfig = (env, key) => process.env[env] ? process.env[env] : config.get(key);

/*
 * Be sure to setup your config values before running this code. You can
 * set them using environment variables or modifying the config file in /config.
 */

exports.APP_CONFIG = {
    // App Secret can be retrieved from the App Dashboard
    APP_SECRET: getConfig('MESSENGER_APP_SECRET', 'appSecret'),
    // Arbitrary value used to validate a webhook
    VALIDATION_TOKEN: getConfig('MESSENGER_VALIDATION_TOKEN', 'validationToken'),
    // Generate a page access token for your page from the App Dashboard
    PAGE_ACCESS_TOKEN: getConfig('MESSENGER_PAGE_ACCESS_TOKEN', 'pageAccessToken'),
    // ID of the page associated with this bot.
    PAGE_ID: getConfig('PAGE_ID', 'pageId'),
    RETHINKDB: config.get('rethinkdb')
};

exports.validate = () => {
    const c = exports.APP_CONFIG;
    if (!(c.APP_SECRET && c.VALIDATION_TOKEN && c.PAGE_ACCESS_TOKEN && c.RETHINKDB)) {
        throw new Error('Missing config values');
    }
};
