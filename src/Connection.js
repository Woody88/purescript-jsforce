exports._newConnection = function(psforce, props) {
    return new psforce.Connection(props);
}