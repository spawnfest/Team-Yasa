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
                if($(this).data('clicked') === true){
                    $.dashboard.bullet.send(JSON.stringify({method: 'unregister', key: $(this).data('name')}));
                    var serie = $.dashboard.chart.get($(this).data('name'));
                    serie.remove(true);
                    $(this).data('clicked', false); 
                } else {
                    $.dashboard.bullet.send(JSON.stringify({method: 'register', key: $(this).data('name'), range: '-40sec'}))
                    //$.dashboard.graph( $(this).data('name'));
                    $(this).data('clicked', true); 
                }
                return false;
            });
            $.dashboard.websocket_init();
            $.dashboard.chart_init();
        },
        
        websocket_init : function() {
            $.dashboard.bullet = $.bullet('ws://'+location.hostname+(location.port ? ':'+location.port: '')+'/wsapi');
            $.dashboard.bullet.onopen = function(){
                console.log('WebSocket: opened');
            };
            $.dashboard.bullet.onclose = function(){
                console.log('WebSocket: closed');
            };
            $.dashboard.bullet.onmessage = function(e){
                if(e.data === 'pong'){
                } else if (e.data === 'ok') {    
                } else if (e.data === 'error:invalid request') {
                } else {
                    var obj = jQuery.parseJSON(e.data);
                    for (x in obj) {
                        var key = obj[x].key;
                        var values = obj[x].values;
                        values = values.splice(-30);
                        values = $.map(values, function(arr) { return [[arr[0] * 1000, arr[1]]] });
                        var initial = obj[x].init;
                        if(initial === true){

                            $.dashboard.chart.addSeries({id: key, name:key, data:values});
                        } else {
                            var serie = $.dashboard.chart.get(key);
                            var shift = serie.data.length >= 30;
                            var laststamp = serie.data[serie.data.length-1].x;
                            for(y in values){
                                var value = values[y];
                                if(value[0] > laststamp){
                                    serie.addPoint(value, true, shift)
                                }
                            }
                        }
                    }
                }  
            };
            $.dashboard.bullet.onheartbeat = function(){
                $.dashboard.bullet.send('ping');
            };
        },

        chart_init: function() {
            $.dashboard.chart = new Highcharts.Chart({
                chart: {
                    renderTo: 'graph',
                    type: 'spline',
                    animation: false
                },
                title: {
                    text: 'Awesome WS Demo'
                },
                xAxis: {
                    type: 'datetime'
                },
                yAxis: {
                    title: ''
                },
            });
        }


    }
})(jQuery);
