exports.mkClient = function(){
  return function(configs) {
    return function() {
        var jsforce = require('jsforce');
        var client = new jsforce.browser.Client();
        client.init(configs);
        return client;
    };
  }
}

exports.login_ = function(){
  return function(client, options, cancelled, error, success){
    return function(onError, onSuccess) {
      client.login(options, function(err, res) {
        if (err) {
          return onSuccess(error(err.message));
        }
  
        if (res.status === 'connect') {
          return onSuccess(success(client.connection));
        }
  
        if (res.status === 'cancel') {
          return onSuccess(cancelled);
        }
        
        return onSuccess(error('Unknown status encountered'));
      });
  
      return function(cancelError, onCancelerError, onCancelerSuccess) {
          onCancelerSuccess();
      };
    }
  }
}


exports.getAccounts = function(conn){
  return function() {
    conn.query("SELECT Id, Name FROM Account LIMIT 20", function(err, result) {
      if (err) { return console.error(err); }
      console.log("total : " + result.totalSize);
      console.log("fetched : " + result.records.length);
    });
    return {}
  }
}

