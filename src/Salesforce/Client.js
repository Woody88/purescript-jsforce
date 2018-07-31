exports.mkClient = function(configs) {
    return function() {
        var jsforce = require('jsforce');
        var client = new jsforce.browser.Client();
        client.init(configs);
        return client;
    };
}

exports.login_ = function(client, options, cancelled, error, success){
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