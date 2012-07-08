(function($) {
    $.dashboard = {
        timer : null,
        data : null,
        lastval : 0,

        init : function() {
            $.ajaxSetup({
                async: false
            });

            $.getJSON("/api/keys", function(data) {
                $('#tree').jqxTree({ source: data, height: '600px', width: '300px' })
            });

            $('#tree .keyname').click(function() {
                $('#timeframe').show();
                $.dashboard.graph( $(this).data('name') );
            });
        },

        graph : function(key, range) {
            clearTimeout($.dashboard.timer);
            range = "-30sec";

            var options = {
                chart: {
                    renderTo: 'graph',
                    type: 'spline'
                },
                title: {
                    text: key
                },
                yAxis: {
                    title: ''
                },
                xAxis: {
                    type: 'datetime'
                }
            };

            $.dashboard.key = key;
            $.dashboard.range = range;

            $.getJSON("/api/get?range="+range+"&key="+key, function(data) {
                data = $.map(data, function(arr) { return [[arr[0] * 1000, arr[1]]] });
                options.series = [{name: key, data: data}];
                $.dashboard.chart = new Highcharts.Chart(options);
            });

            $.dashboard.timer = setInterval("$.dashboard.update()", 1000);
        },

        update : function() {
            $.getJSON("/api/get?range=-1sec&key="+$.dashboard.key, function(data) {
                data = $.map(data, function(arr) { return [[arr[0] * 1000, arr[1]]] });
                shift = $.dashboard.chart.series[0].data.length >= 30;
                $.dashboard.chart.series[0].addPoint(data[0], true, shift);
            });
        }
    }
})(jQuery);
