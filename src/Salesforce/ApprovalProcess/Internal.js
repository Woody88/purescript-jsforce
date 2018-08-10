exports.mkApprovalProcess = function(conn){
    return function(){
        return conn.process.approval;
    }
} 

exports.submitImpl = function(){
    return function(ap, contextId, comments, options, error, success){
        return function(onError, onSuccess){
            var submissionError = function(err){ 
                var errorMsg = "Error-" + err.name + " ErrorCode-" + err.errorCode + " Fields: "+ err.fields.join();
                return onSuccess( error( errorMsg ) ); 
            }
            var submissionSuccess = function(apReqRes){ 
                apReqRes.errors = apReqRes.errors.map(function(e) { return e.message; });
                return onSuccess( success( apReqRes ) ); 
            }
            
            ap.submit(contextId, comments, options).then(submissionSuccess, submissionError);
            return function(cancelError, onCancelerError, onCancelerSuccess) {
                onCancelerSuccess();
            }
        }
    }
}