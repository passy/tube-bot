'use strict';

const crypto = require('crypto');
const bodyParser = require('body-parser');
const express = require('express');
const rethink = require('rethinkdbdash');

const config = require('./appconfig');
const messenger = require('./messenger');
const bot = require('./output/Bot');

bot.handleMessage();

const app = express();
app.set('port', process.env.PORT || 5000);
app.set('view engine', 'ejs');
app.use(bodyParser.json({verify: verifyRequestSignature}));
app.use(express.static('public'));

config.validate();

const APP_CONFIG = config.APP_CONFIG;
const r = rethink(APP_CONFIG.RETHINKDB);

/*
 * Use your own validation token. Check that the token used in the Webhook
 * setup is the same token used here.
 *
 */
app.get('/webhook', (req, res) => {
    if (req.query['hub.mode'] === 'subscribe' &&
    req.query['hub.verify_token'] === APP_CONFIG.VALIDATION_TOKEN) {
        console.log('Validating webhook');
        res.status(200).send(req.query['hub.challenge']);
    } else {
        console.error('Failed validation. Make sure the validation tokens match.');
        res.sendStatus(403);
    }
});

/*
 * All callbacks for Messenger are POST-ed. They will be sent to the same
 * webhook. Be sure to subscribe your app to your page to receive callbacks
 * for your page.
 * https://developers.facebook.com/docs/messenger-platform/product-overview/setup#subscribe_app
 *
 */
app.post('/webhook', (req, res) => {
    const data = req.body;

     // Make sure this is a page subscription
    if (data.object === 'page') {
         // Iterate over each entry
         // There may be multiple if batched
        data.entry.forEach(pageEntry => {
            const pageId = pageEntry.id;

            if (pageId !== APP_CONFIG.PAGE_ID) {
                return;
            }

             // Iterate over each messaging event
            pageEntry.messaging.forEach(function (messagingEvent) {
                if (messagingEvent.optin) {
                    messenger.receivedAuthentication(messagingEvent);
                } else if (messagingEvent.message) {
                    messenger.receivedMessage(r, messagingEvent);
                } else if (messagingEvent.delivery) {
                    messenger.receivedDeliveryConfirmation(messagingEvent);
                } else if (messagingEvent.postback) {
                    messenger.receivedPostback(messagingEvent);
                } else if (messagingEvent.read) {
                    messenger.receivedMessageRead(messagingEvent);
                } else if (messagingEvent.account_linking) {
                    messenger.receivedAccountLink(messagingEvent);
                } else {
                    console.log('Webhook received unknown messagingEvent: ', messagingEvent);
                }
            });
        });

         // Assume all went well.
         //
         // You must send back a 200, within 20 seconds, to let us know you've
         // successfully received the callback. Otherwise, the request will time out.
        res.sendStatus(200);
    }
});

function constEq(a, b, minComp) {
    const aLen = a.length;
    const bLen = b.length;
    const len = Math.max(aLen, bLen, minComp || 0);
    let ret = 0;

    for (let i = 0; i < len; i++) {
        ret |= a[i % aLen] ^ b[i % bLen];
    }

    ret |= aLen ^ bLen;

    return ret === 0;
}

/*
 * Verify that the callback came from Facebook. Using the App Secret from
 * the App Dashboard, we can verify the signature that is sent with each
 * callback in the x-hub-signature field, located in the header.
 *
 * https://developers.facebook.com/docs/graph-api/webhooks#setup
 *
 */
function verifyRequestSignature(req, res, buf) {
    const signature = req.headers['x-hub-signature'];

    if (!signature) {
        throw new Error('Couldn\'t validate the signature.');
    } else {
        const elements = signature.split('=');
        const signatureHash = elements[1];

        const expectedHash = crypto.createHmac('sha1', APP_CONFIG.APP_SECRET)
                        .update(buf)
                        .digest('hex');

        if (!constEq(signatureHash, expectedHash)) {
            throw new Error('Couldn\'t validate the request signature.');
        }
    }
}

// Start server
// Webhooks must be available via SSL with a certificate signed by a valid
// certificate authority.
app.listen(app.get('port'), function () {
    console.log('Node app is running on port', app.get('port'));

    r.table('disruptions').changes().run((err, cursor) => {
        if (err) {
            throw err;
        }

        cursor.each((err, row) => {
            if (err) {
                return;
            }

            if (row !== null) {
                sendDisruptionNotification(row.new_val);
            }
        });
    });
});

function sendDisruptionNotification(disruption) {
    r.table('messenger_subscriptions')
     .filter(r.row('status').eq('subscribed'))
     .run((err, cursor) => {
         if (err) {
             throw err;
         }

         cursor.forEach(row => {
             messenger.sendTextMessage(
                 row.recipient_id, `New Disruption Alert on ${disruption.name} line!`);
         });
     });
}

module.exports = app;