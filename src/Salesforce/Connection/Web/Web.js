"use strict";

exports.sforceConnected = function(e){
    console.log("Added");
    return e.detail.connection;
}


exports.sforceConnectedEvent = function(eventName){
    return function(connection){
        return new CustomEvent(eventName, {detail: { connection: connection }});
    }
}

exports.sforceConnectedImpl = function(e){
    console.log("Added");
    return e.detail.connection;
}


exports.sforceConnectedEventImpl = function(eventName){
    return function(connection){
        return new CustomEvent(eventName, {detail: { connection: connection }});
    }
}

exports.windowOpener = function(){
    return window.opener == null ? window : window.opener;
}