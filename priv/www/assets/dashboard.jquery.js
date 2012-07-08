(function($) {
    $.dashboard = {
        init : function() {
            $.ajaxSetup({
                async: false
            });

            $.getJSON("/api/keys", function(data) {
                    $('#keys').jqxTree({ source: data, height: '600px', width: '300px' })
                    //$("#keys").append("<li data-name='"+key+"'><a href='#'>" + key + "</a></li>");
            });

            $('#keys .keyname').click(function() {
                $.dashboard.graph( $(this).data('name') );
                return false;
            });
        },

        graph : function(key) {
            var options = {
                chart: {
                    renderTo: 'graph',
                    type: 'area'
                },
                title: {
                    text: key
                },
                xAxis: {
                    type: 'datetime'
                }
            };

            $.getJSON("/api/get?range=-5min&key="+key, function(data) {
                data = $.map(data, function(arr) { return [[arr[0] * 1000, arr[1]]] });
                console.log(data);
                
                options.series = [{name: key, data: data}];
                new Highcharts.Chart(options);
            });
        }
    }
})(jQuery);
