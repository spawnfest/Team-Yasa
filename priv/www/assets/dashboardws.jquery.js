(function($) {
    var bullet = $.bullet('ws://localhost:8080/wsapi');
    bullet.onopen = function(){
        console.log('WebSocket: opened');
    };
    bullet.onclose = function(){
        console.log('WebSocket: closed');
    };
    bullet.onmessage = function(e){
        if(e.data === 'pong'){
        } else if (e.data === 'ok') {    
        } else if (e.data === 'error:invalid request') {
        } else {
            var obj = jQuery.parseJSON(e.data);
            for (x in obj) {
                var key = obj[x].key;
                var values = obj[x].values;
            }
        }  
    };
    bullet.onheartbeat = function(){
        bullet.send('ping');
    };

    var chart;
    $(document).ready(function() {
        chart = new Highcharts.Chart({
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
    });
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
                    bullet.send(JSON.stringify({method: 'unregister', key: $(this).data('name')}));
                    $(this).data('clicked', false); 
                } else {
                    bullet.send(JSON.stringify({method: 'register', key: $(this).data('name'), range: '-5min'}))
                    //$.dashboard.graph( $(this).data('name'));
                    $(this).data('clicked', true); 
                }
                return false;
            });
        },
        
        graph : function(key, range) {
            clearTimeout(this.timer);
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
            $.dashboard.chart = new Highcharts.Chart(options);
        },
    }
})(jQuery);
