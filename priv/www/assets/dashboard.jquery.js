(function($) {
    $.dashboard = {
        websocket : null,

        init : function() {
            this.setup_bindings();

            $.ajaxSetup({
                async: false
            });

            $.getJSON("/api/keys", function(data) {
                console.log(data);
                $('#tree').jqxTree({ source: data, height: '600px', width: '300px' })
            });

            $('#tree .keyname').click(function() {
                $('#timeframe').show();
                $.dashboard.graph( $(this).data('name') );
            });
        },

        websocket_init : function(onmsg_handler) {
            var current_host = document.location.hostname;
            var port = document.location.port;
            var fulldomain = current_host + ":" + port;

            if ('WebSocket' in window) {
                this.websocket = new WebSocket('ws://'+fulldomain+'/api/get?range=-5min&key=stats.binary');

                this.websocket.onopen = function() {
                    console.log("[DEBUG] Websocket Connected");
                }
                this.websocket.onmessage = onmsg_handler;
            }

            return this.websocket;
        },

        graph : function(key, range) {
            range = (range ? range : "-5min");

            var options = {
                chart: {
                    renderTo: 'graph',
                    type: 'area'
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

            $.getJSON("/api/get?range="+range+"&key="+key, function(data) {
                data = $.map(data, function(arr) { return [[arr[0] * 1000, arr[1]]] });
                console.log(data);
                
                options.series = [{name: key, data: data}];
                new Highcharts.Chart(options);
            });
        },

        setup_bindings : function() {
            $('#timeframe li').click(function() {
                alert('works');
            });
        },

        handle_time_change : function(obj) {
            console.log(obj);
        }

    }
})(jQuery);
