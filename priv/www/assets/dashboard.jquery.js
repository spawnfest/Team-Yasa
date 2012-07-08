(function($) {
    $.dashboard = {
        ENDPOINT : "http://localhost:8080/",

        init : function() {
            $.ajaxSetup({
                async: false
            });

            $.getJSON(this.ENDPOINT + "api/keys", function(data) {
                $.each(data, function(i, key) {
                    $("#keys").append("<li>" + key + "</li>");
                });
            });

            console.log(this.ENDPOINT);
        }
    }
})(jQuery);
