exports.mkConnection_ = function() {
    return function(configs){
        return function(){
            var jsforce = require('jsforce');
            return new jsforce.Connection(configs);
        }
    }
}

exports.login_ = function(conn, user, pass, loginErr, tuple, success){
    return function(onError, onSuccess) {
        conn.login(user, pass, function(err, userInfo){
            if (err) {
                return onSuccess(loginErr(err.message));
              }
            return onSuccess( success( tuple(conn)(userInfo) ) );
        });

        return function(cancelError, onCancelerError, onCancelerSuccess) {
            onCancelerSuccess();
        };
    }
}

exports.logout_ = function(conn){
    return function(){
        conn.logout();
        return {};
    }
  }
  