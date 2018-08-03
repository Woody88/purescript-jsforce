const objs = {"totalSize":10,
              "done":true,
              "records": [{"attributes":{"type":"Account","url":"/services/data/v42.0/sobjects/Account/00110000011280xAAA"},"Id":"00110000011280xAAA","Name":"出船鮨"},{"attributes":{"type":"Account","url":"/services/data/v42.0/sobjects/Account/00110000011280yAAA"},"Id":"00110000011280yAAA","Name":"加藤精肉店"
              },{"attributes":{"type":"Account","url":"/services/data/v42.0/sobjects/Account/00110000011280zAAA"},"Id":"00110000011280zAAA","Name":"加固商店"},{"attributes":{"type":"Account","url":"/services/data/v42.0/sobjects/Account/001100000112810AAA"},"Id":"001100000112810AAA","Name":"鈴昇建設"},{"attributes":{"type":"Account","url":"/services/data/v42.0/sobjects/Account/001100000112811AAA"},"Id":"001100000112811AAA","Name":"勝田建築"},{"attributes":{"type":"Account","url":"/services/data/v42.0/sobjects/Account/001100000112812AAA"},"Id":"001100000112812AAA","Name":"広伝製麺所"},{"attributes":{"type":"Account","url":"/services/data/v42.0/sobjects/Account/001100000112813AAA"},"Id":"001100000112813AAA","Name":"関川タイヤ商会"},{"attributes":{"type":"Account","url":"/services/data/v42.0/sobjects/Account/001100000112814AAA"},"Id":"001100000112814AAA","Name":"喜楽"},{"attributes":{"type":"Account","url":"/services/data/v42.0/sobjects/Account/001100000112815AAA"},"Id":"001100000112815AAA","Name":"渡辺建設"},{"attributes":{"type":"Account","url":"/services/data/v42.0/sobjects/Account/001100000112816AAA"},"Id":"001100000112816AAA","Name":"吉岡建築"}]}

exports.queryString_ = function(conn, q, error, success){
    return function(onError, onSuccess){
        // conn.query(q, function(err, res){
        //     if (err){
        //         console.log('FFI queryString Error: ', err);
        //         return onSucess( error( err.message ) );
        //     }
            
        //     //console.log('FFI queryString Result: ', res);
        //     return onSuccess( sucesss( objs ) );
            
        // });
        setTimeout(function(){ onSuccess( sucesss( objs ) ); }, 2000);
        return onSuccess( sucesss( objs ) );

        return function(cancelError, onCancelerError, onCancelerSuccess) {
            onCancelerSuccess();
        };
    }
}

exports.runSOQL_ = function(conn, q, error, success){
    return function(onError, onSuccess){
        conn.query(q, function(err, res){
            if (err){
                //console.log('FFI queryString Error: ', err);
                return onError( error( err.message ) );
            }
            
            console.log('FFI queryString Result: ', JSON.stringify(res));
            return onSuccess( sucesss( res ) );
            
        });

        return function(cancelError, onCancelerError, onCancelerSuccess) {
            onCancelerSuccess();
        };
    }
}