'use strict';

/*
 * Authorization Event
 *
 * The value for 'optin.ref' is defined in the entry point. For the "Send to
 * Messenger" plugin, it is the 'data-ref' field. Read more at
 * https://developers.facebook.com/docs/messenger-platform/webhook-reference/authentication
 *
 */
exports.receivedAuthentication = event => {
    const senderID = event.sender.id;
    const recipientID = event.recipient.id;
    const timeOfAuth = event.timestamp;

    // The 'ref' field is set in the 'Send to Messenger' plugin, in the 'data-ref'
    // The developer can set this to an arbitrary value to associate the
    // authentication callback with the 'Send to Messenger' click event. This is
    // a way to do account linking when the user clicks the 'Send to Messenger'
    // plugin.
    const passThroughParam = event.optin.ref;

    console.log(`Received authentication for user %d and page %d with pass
                 through param \'%s\' at %d`,
                 senderID, recipientID, passThroughParam, timeOfAuth);
};

/*
 * Delivery Confirmation Event
 *
 * This event is sent to confirm the delivery of a message. Read more about
 * these fields at https://developers.facebook.com/docs/messenger-platform/webhook-reference/message-delivered
 *
 */
exports.receivedDeliveryConfirmation = event => {
    const senderID = event.sender.id;
    const recipientID = event.recipient.id;
    const delivery = event.delivery;
    const messageIDs = delivery.mids;
    const watermark = delivery.watermark;
    const sequenceNumber = delivery.seq;

    if (messageIDs) {
        messageIDs.forEach(messageID => {
            console.log('Received delivery confirmation from sender %s ' +
                        'to recipient % s for message ID %s and sequence number %s',
            senderID, recipientID, messageID, sequenceNumber);
        });
    }

    console.log('All message before %d were delivered.', watermark);
};

/*
 * Postback Event
 *
 * This event is called when a postback is tapped on a Structured Message.
 * https://developers.facebook.com/docs/messenger-platform/webhook-reference/postback-received
 *
 */
exports.receivedPostback = event => {
    const senderID = event.sender.id;
    const recipientID = event.recipient.id;
    const timeOfPostback = event.timestamp;

    // The 'payload' param is a developer-defined field which is set in a postback
    // button for Structured Messages.
    const payload = event.postback.payload;

    console.log('Received postback for user %d and page %d with payload \'%s\' ' +
        'at %d', senderID, recipientID, payload, timeOfPostback);
};

/*
 * Message Read Event
 *
 * This event is called when a previously-sent message has been read.
 * https://developers.facebook.com/docs/messenger-platform/webhook-reference/message-read
 *
 */
exports.receivedMessageRead = event => {
    const senderID = event.sender.id;
    const recipientID = event.recipient.id;

    // All messages before watermark (a timestamp) or sequence have been seen.
    const watermark = event.read.watermark;
    const sequenceNumber = event.read.seq;

    console.log('Received message read event from %s to %s for watermark ' +
                '%d and sequence number %d',
                senderID, recipientID, watermark, sequenceNumber);
};

/*
 * Account Link Event
 *
 * This event is called when the Link Account or UnLink Account action has been
 * tapped.
 * https://developers.facebook.com/docs/messenger-platform/webhook-reference/account-linking
 *
 */
exports.receivedAccountLink = event => {
    const senderID = event.sender.id;
    const recipientID = event.recipient.id;

    const status = event.account_linking.status;
    const authCode = event.account_linking.authorization_code;

    console.log('Received account link event with for user %d from %s ' +
                'with status %s and auth code %s ',
                senderID, recipientID, status, authCode);
};
