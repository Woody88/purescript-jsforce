exports.queryString_ = function(conn, q, error, success){
    return function(onError, onSuccess){
        conn.query(q, function(err, res){
            if (err){
                console.log('FFI queryString Error: ', err);
                return onSucess( error( err.message ) );
            }
            
            console.log('FFI queryString Result: ', res);
            return onSuccess( sucesss( res ) );
            
        });

        return function(cancelError, onCancelerError, onCancelerSuccess) {
            onCancelerSuccess();
        };
    }
}
