exports.retrieve_ = function(conn, type, ids, error, success){
    return function(onError, onSuccess) {
        var retrieveError = function(err){ return onSuccess( error( err.message ) ); }
        var retrieveSuccess = function(records){ return onSuccess( success( records ) ); }
            
        conn.retrieve(type, ids).then(retrieveSuccess, retrieveError);

        return function(cancelError, onCancelerError, onCancelerSuccess) {
            onCancelerSuccess();
        };
    }
}

exports.update_ = function(conn, type, records,error, success){
    return function(onError, onSuccess) {
        var updateError = function(err){ return onSuccess( error( err.message ) ); }
        var updateSuccess = function(res){ return onSuccess( success( res ) ); }
            
        conn.update(type, records).then(updateSuccess, updateError);

        return function(cancelError, onCancelerError, onCancelerSuccess) {
            onCancelerSuccess();
        };
    }
}

exports.create_ = function(conn, type, records,error,success){
    return function(onError, onSuccess) {
        var createError = function(err){ return onSuccess( error( err.message ) ); }
        var createSuccess = function(res){ return onSuccess( success( res ) ); }
            
        conn.create(type, records).then(createSuccess, createError);

        return function(cancelError, onCancelerError, onCancelerSuccess) {
            onCancelerSuccess();
        };
    }
}

exports.destroy_ = function(conn, type, ids, error, success){
    return function(onError, onSuccess) {
        var destroyError = function(err){ return onSuccess( error( err.message ) ); }
        var destroySuccess = function(records){ return onSuccess( success( records ) ); }
            
        conn.destroy(type, ids).then(destroySuccess, destroyError);

        return function(cancelError, onCancelerError, onCancelerSuccess) {
            onCancelerSuccess();
        };
    }
}