"use strict";

exports.sforceConnected = function(e){
    return e.sforceConnected;
}


exports.sforceConnectedEvent = function(eventName){
    return function(connection){
        return new CustomEvent(eventName, connection);
    }
}