exports.getImpl = function(conn, path, opts, error, success){
    return function(onError, onSuccess){
        console.log("in");
        var getSuccess = function(res){ console.log("out1"); return onSuccess( success( res ) ); }
        var getError = function(err){ console.log("out2"); return onSuccess(  error( err.name ) ); }
        conn.apex.get(path, opts).then(getSuccess, getError);

        return function(cancelError, onCancelerError, onCancelerSuccess){
            onCancelerSuccess();
        }
    }
}