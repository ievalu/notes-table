$("#deleteFile").change(function() {
    if ($("#deleteFile").prop("checked")) {
        $("#file-input").css("display", "none");
    } else {
        $("#file-input").css("display", "inline")
    }
});