/* eslint-disable camelcase, no-use-before-define, max-lines, no-unused-vars */
'use strict';

const request = require('request');
const APP_CONFIG = require('./appconfig').APP_CONFIG;

/*
 * Call the Send API. The message data goes in the body. If successful, we'll
 * get the message id in a response
 *
 */
const callSendAPI = exports.callSendAPI = messageData => {
    request({
        uri: 'https://graph.facebook.com/v2.6/me/messages',
        qs: {access_token: APP_CONFIG.PAGE_ACCESS_TOKEN},
        method: 'POST',
        json: messageData

    }, function (error, response, body) {
        if (!error && response.statusCode === 200) {
            const recipientId = body.recipient_id;
            const messageId = body.message_id;

            if (messageId) {
                console.log('Successfully sent message with id %s to recipient %s',
                    messageId, recipientId);
            } else {
                console.log('Successfully called Send API for recipient %s',
                    recipientId);
            }
        } else {
            console.error(response.error);
        }
    });
};

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

    // When an authentication is received, we'll send a message back to the sender
    // to let them know it was successful.
    sendTextMessage(senderID, 'Authentication successful');
};

/*
 * Message Event
 *
 * This event is called when a message is sent to your page. The 'message'
 * object format can vary depending on the kind of message that was received.
 * Read more at https://developers.facebook.com/docs/messenger-platform/webhook-reference/message-received
 *
 * For this example, we're going to echo any text that we get. If we get some
 * special keywords ('button', 'generic', 'receipt'), then we'll send back
 * examples of those bubbles to illustrate the special message bubbles we've
 * created. If we receive a message with an attachment (image, video, audio),
 * then we'll simply confirm that we've received the attachment.
 *
 */
const receivedMessage = exports.receivedMessage = (r, event) => {
    console.log(event);
    const senderID = event.sender.id;
    const recipientID = event.recipient.id;
    const timeOfMessage = event.timestamp;
    const message = event.message;

    console.log('Received message for user %d and page %d at %d with message:',
        senderID, recipientID, timeOfMessage);
    console.log(JSON.stringify(message));

    const isEcho = message.is_echo;
    const messageId = message.mid;
    const appId = message.app_id;
    const metadata = message.metadata;

    // You may get a text or attachment but not both
    const messageText = message.text;
    const messageAttachments = message.attachments;
    const quickReply = message.quick_reply;

    if (isEcho) {
        // Just logging message echoes to console
        console.log('Received echo for message %s and app %d with metadata %s',
            messageId, appId, metadata);
        return;
    } else if (quickReply) {
        const quickReplyPayload = quickReply.payload;
        console.log('Quick reply for message %s with payload %s',
            messageId, quickReplyPayload);

        sendTextMessage(senderID, 'Quick reply tapped');
        return;
    }

    if (messageText) {
        const cmd = messageText.split(/\s/);
        // If we receive a text message, check to see if it matches any special
        // keywords and send back the corresponding example. Otherwise, just echo
        // the text we received.
        switch (cmd[0]) {
            case 'image':
                sendImageMessage(senderID);
                break;

            case 'gif':
                sendGifMessage(senderID);
                break;

            case 'audio':
                sendAudioMessage(senderID);
                break;

            case 'video':
                sendVideoMessage(senderID);
                break;

            case 'file':
                sendFileMessage(senderID);
                break;

            case 'button':
                sendButtonMessage(senderID);
                break;

            case 'generic':
                sendGenericMessage(senderID);
                break;

            case 'receipt':
                sendReceiptMessage(senderID);
                break;

            case 'quickreply':
                sendQuickReply(senderID);
                break;

            case 'readreceipt':
                sendReadReceipt(senderID);
                break;

            case 'typingon':
                sendTypingOn(senderID);
                break;

            case 'typingoff':
                sendTypingOff(senderID);
                break;

            case 'accountlinking':
                sendAccountLinking(senderID);
                break;

            default:
                sendTextMessage(senderID, messageText);
        }
    } else if (messageAttachments) {
        sendTextMessage(senderID, 'Message with attachment received');
    }
};

/*
 * Delivery Confirmation Event
 *
 * This event is sent to confirm the delivery of a message. Read more about
 * these fields at https://developers.facebook.com/docs/messenger-platform/webhook-reference/message-delivered
 *
 */
const receivedDeliveryConfirmation = exports.receivedDeliveryConfirmation = event => {
    const senderID = event.sender.id;
    const recipientID = event.recipient.id;
    const delivery = event.delivery;
    const messageIDs = delivery.mids;
    const watermark = delivery.watermark;
    const sequenceNumber = delivery.seq;

    if (messageIDs) {
        messageIDs.forEach(messageID => {
            console.log('Received delivery confirmation for message ID: %s',
            messageID);
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
const receivedPostback = exports.receivedPostback = event => {
    const senderID = event.sender.id;
    const recipientID = event.recipient.id;
    const timeOfPostback = event.timestamp;

    // The 'payload' param is a developer-defined field which is set in a postback
    // button for Structured Messages.
    const payload = event.postback.payload;

    console.log('Received postback for user %d and page %d with payload \'%s\' ' +
        'at %d', senderID, recipientID, payload, timeOfPostback);

    // When a postback is called, we'll send a message back to the sender to
    // let them know it was successful
    sendTextMessage(senderID, 'Postback called');
};

/*
 * Message Read Event
 *
 * This event is called when a previously-sent message has been read.
 * https://developers.facebook.com/docs/messenger-platform/webhook-reference/message-read
 *
 */
const receivedMessageRead = exports.receivedMessageRead = event => {
    const senderID = event.sender.id;
    const recipientID = event.recipient.id;

    // All messages before watermark (a timestamp) or sequence have been seen.
    const watermark = event.read.watermark;
    const sequenceNumber = event.read.seq;

    console.log('Received message read event for watermark %d and sequence ' +
        'number %d', watermark, sequenceNumber);
};

/*
 * Account Link Event
 *
 * This event is called when the Link Account or UnLink Account action has been
 * tapped.
 * https://developers.facebook.com/docs/messenger-platform/webhook-reference/account-linking
 *
 */
const receivedAccountLink = exports.receivedAccountLink = event => {
    const senderID = event.sender.id;
    const recipientID = event.recipient.id;

    const status = event.account_linking.status;
    const authCode = event.account_linking.authorization_code;

    console.log('Received account link event with for user %d with status %s ' +
    'and auth code %s ', senderID, status, authCode);
};

/*
 * Send an image using the Send API.
 *
 */
const sendImageMessage = exports.sendImageMessage = recipientId => {
    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            attachment: {
                type: 'image',
                payload: {
                    url: APP_CONFIG.SERVER_URL + '/assets/rift.png'
                }
            }
        }
    };

    callSendAPI(messageData);
};

/*
 * Send a Gif using the Send API.
 *
 */
const sendGifMessage = exports.sendGifMessage = recipientId => {
    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            attachment: {
                type: 'image',
                payload: {
                    url: APP_CONFIG.SERVER_URL + '/assets/instagram_logo.gif'
                }
            }
        }
    };

    callSendAPI(messageData);
};

/*
 * Send audio using the Send API.
 *
 */
const sendAudioMessage = exports.sendAudioMessage = recipientId => {
    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            attachment: {
                type: 'audio',
                payload: {
                    url: APP_CONFIG.SERVER_URL + '/assets/sample.mp3'
                }
            }
        }
    };

    callSendAPI(messageData);
};

/*
 * Send a video using the Send API.
 *
 */
const sendVideoMessage = exports.sendVideoMessage = recipientId => {
    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            attachment: {
                type: 'video',
                payload: {
                    url: APP_CONFIG.SERVER_URL + '/assets/allofus480.mov'
                }
            }
        }
    };

    callSendAPI(messageData);
};

/*
 * Send a video using the Send API.
 *
 */
const sendFileMessage = exports.sendFileMessage = recipientId => {
    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            attachment: {
                type: 'file',
                payload: {
                    url: APP_CONFIG.SERVER_URL + '/assets/test.txt'
                }
            }
        }
    };

    callSendAPI(messageData);
};

/*
 * Send a text message using the Send API.
 *
 */
const sendTextMessage = exports.sendTextMessage = (recipientId, messageText) => {
    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            text: messageText,
            metadata: 'DEVELOPER_DEFINED_METADATA'
        }
    };

    callSendAPI(messageData);
};

/*
 * Send a button message using the Send API.
 *
 */
function sendButtonMessage(recipientId) {
    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            attachment: {
                type: 'template',
                payload: {
                    template_type: 'button', // eslint-disable-line camelcase
                    text: 'This is test text',
                    buttons: [{
                        type: 'web_url',
                        url: 'https://www.oculus.com/en-us/rift/',
                        title: 'Open Web URL'
                    }, {
                        type: 'postback',
                        title: 'Trigger Postback',
                        payload: 'DEVELOPED_DEFINED_PAYLOAD'
                    }, {
                        type: 'phone_number',
                        title: 'Call Phone Number',
                        payload: '+16505551234'
                    }]
                }
            }
        }
    };

    callSendAPI(messageData);
}

/*
 * Send a Structured Message (Generic Message type) using the Send API.
 *
 */
const sendGenericMessage = exports.sendGenericMessage = recipientId => {
    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            attachment: {
                type: 'template',
                payload: {
                    template_type: 'generic',
                    elements: [{
                        title: 'rift',
                        subtitle: 'Next-generation virtual reality',
                        item_url: 'https://www.oculus.com/en-us/rift/',
                        image_url: APP_CONFIG.SERVER_URL + '/assets/rift.png',
                        buttons: [{
                            type: 'web_url',
                            url: 'https://www.oculus.com/en-us/rift/',
                            title: 'Open Web URL'
                        }, {
                            type: 'postback',
                            title: 'Call Postback',
                            payload: 'Payload for first bubble'
                        }]
                    }, {
                        title: 'touch',
                        subtitle: 'Your Hands, Now in VR',
                        item_url: 'https://www.oculus.com/en-us/touch/',
                        image_url: APP_CONFIG.SERVER_URL + '/assets/touch.png',
                        buttons: [{
                            type: 'web_url',
                            url: 'https://www.oculus.com/en-us/touch/',
                            title: 'Open Web URL'
                        }, {
                            type: 'postback',
                            title: 'Call Postback',
                            payload: 'Payload for second bubble'
                        }]
                    }]
                }
            }
        }
    };

    callSendAPI(messageData);
};

/*
 * Send a receipt message using the Send API.
 *
 */
const sendReceiptMessage = exports.sendReceiptMessage = recipientId => {
    // Generate a random receipt ID as the API requires a unique ID
    const receiptId = 'order' + Math.floor(Math.random() * 1000);

    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            attachment: {
                type: 'template',
                payload: {
                    template_type: 'receipt',
                    recipient_name: 'Peter Chang',
                    order_number: receiptId,
                    currency: 'USD',
                    payment_method: 'Visa 1234',
                    timestamp: '1428444852',
                    elements: [{
                        title: 'Oculus Rift',
                        subtitle: 'Includes: headset, sensor, remote',
                        quantity: 1,
                        price: 599.00,
                        currency: 'USD',
                        image_url: APP_CONFIG.SERVER_URL + '/assets/riftsq.png'
                    }, {
                        title: 'Samsung Gear VR',
                        subtitle: 'Frost White',
                        quantity: 1,
                        price: 99.99,
                        currency: 'USD',
                        image_url: APP_CONFIG.SERVER_URL + '/assets/gearvrsq.png'
                    }],
                    address: {
                        street_1: '1 Hacker Way',
                        street_2: '',
                        city: 'Menlo Park',
                        postal_code: '94025',
                        state: 'CA',
                        country: 'US'
                    },
                    summary: {
                        subtotal: 698.99,
                        shipping_cost: 20.00,
                        total_tax: 57.67,
                        total_cost: 626.66
                    },
                    adjustments: [{
                        name: 'New Customer Discount',
                        amount: -50
                    }, {
                        name: '$100 Off Coupon',
                        amount: -100
                    }]
                }
            }
        }
    };

    callSendAPI(messageData);
};

/*
 * Send a message with Quick Reply buttons.
 *
 */
const sendQuickReply = exports.sendQuickReply = recipientId => {
    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            text: 'What\'s your favorite movie genre?',
            metadata: 'DEVELOPER_DEFINED_METADATA',
            quick_replies: [
                {
                    content_type: 'text',
                    title: 'Action',
                    payload: 'DEVELOPER_DEFINED_PAYLOAD_FOR_PICKING_ACTION'
                },
                {
                    content_type: 'text',
                    title: 'Comedy',
                    payload: 'DEVELOPER_DEFINED_PAYLOAD_FOR_PICKING_COMEDY'
                },
                {
                    content_type: 'text',
                    title: 'Drama',
                    payload: 'DEVELOPER_DEFINED_PAYLOAD_FOR_PICKING_DRAMA'
                }
            ]
        }
    };

    callSendAPI(messageData);
};

/*
 * Send a read receipt to indicate the message has been read
 *
 */
const sendReadReceipt = exports.sendReadReceipt = recipientId => {
    console.log('Sending a read receipt to mark message as seen');

    const messageData = {
        recipient: {
            id: recipientId
        },
        sender_action: 'mark_seen'
    };

    callSendAPI(messageData);
};

/*
 * Turn typing indicator on
 *
 */
const sendTypingOn = exports.sendTypingOn = recipientId => {
    console.log('Turning typing indicator on');

    const messageData = {
        recipient: {
            id: recipientId
        },
        sender_action: 'typing_on'
    };

    callSendAPI(messageData);
};

/*
 * Turn typing indicator off
 *
 */
const sendTypingOff = exports.sendTypingOff = recipientId => {
    console.log('Turning typing indicator off');

    const messageData = {
        recipient: {
            id: recipientId
        },
        sender_action: 'typing_off'
    };

    callSendAPI(messageData);
};

/*
 * Send a message with the account linking call-to-action
 *
 */
const sendAccountLinking = exports.sendAccountLinking = recipientId => {
    const messageData = {
        recipient: {
            id: recipientId
        },
        message: {
            attachment: {
                type: 'template',
                payload: {
                    template_type: 'button',
                    text: 'Welcome. Link your account.',
                    buttons: [{
                        type: 'account_link',
                        url: APP_CONFIG.SERVER_URL + '/authorize'
                    }]
                }
            }
        }
    };

    callSendAPI(messageData);
};

/*
 * Subscribe a user to all alerts.
 */
const subscribeUser = exports.subscribeUser = (r, recipientId, line) => {
    r.table('messenger_subscriptions')
        .insert(
            {recipient_id: recipientId, status: 'subscribed', line: line},
            {conflict: 'replace'})
        .run(err => {
            if (err) {
                throw err;
            }
            sendTextMessage(recipientId,
                `You're now getting updates for ${line}.`);
        });
};

/*
 * Unsubscribe a user from all alerts.
 */
const unsubscribeUser = exports.unsubscribeUser = (r, recipientId, line) => {
    r.table('messenger_subscriptions')
        .get(recipientId)
        .update({status: 'unsubscribed', line: line})
        .run(err => {
            if (err) {
                throw err;
            }
            sendTextMessage(recipientId,
                 `Sorry for the noise. You're now unsubscribed from ${line} updates.`);
        });
};
