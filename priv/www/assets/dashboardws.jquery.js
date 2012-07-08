(function($) {
    var bullet = $.bullet('ws://localhost:8080/wsapi');
    bullet.onopen = function(){
        console.log('WebSocket: opened');
    };
    bullet.onclose = function(){
        console.log('WebSocket: closed');
    };
    bullet.onmessage = function(e){
        if(e.data !== 'pong'){
            //alert(e.data);
        }        
    };
    bullet.onheartbeat = function(){
        bullet.send('ping');
    };
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
                    bullet.send(JSON.stringify({type: 'unregister', key: $(this).data('name')}));
                    $(this).data('clicked', false); 
                } else {
                    bullet.send(JSON.stringify({type: 'register', key: $(this).data('name'), range: '-5min'}))
                    //$.dashboard.graph( $(this).data('name'));
                    $(this).data('clicked', true); 
                }
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
