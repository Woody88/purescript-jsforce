"use strict";

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

exports.windowClose = function(window){
    return function(){
        return window.close();
    }
}