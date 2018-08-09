exports.mkProcess = function(conn){
    return function(){
        return conn.process;
    }
} 