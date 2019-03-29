
$("#basic-color-select").change(function() {
    var color = $(this).val()
    $("#color-picker").val(color)
    $("#color-picker").css("backgroundColor", color)
});

/*
$("#submit-button").click(function(e) {
    var formData = {
        text: $("#text").val(),
        color: color
    };
    e.preventDefault();
    $.ajax({
      type: "POST",
      url: "http://localhost:9000/notes",
      data: JSON.stringify(formData),
      success: window.location.replace("http://localhost:9000/notes"),
      error: console.log("error"),
      contentType: "application/json"
    });
});*/
