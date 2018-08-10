exports.getImpl = function(conn, path, opts, error, success){
    return function(onError, onSuccess){
        function getSuccess(res){ return onSuccess( success( res ) ); }
        function getError(err){ return onSuccess( error( err.name ) ); }
        conn.apex.get(path, opts).then(getSuccess, getError);

        return function(cancelError, onCancelerError, onCancelerSuccess){
            onSuccessError();
        }
    }
}