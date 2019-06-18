const monthNames = ["January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
];

const date = new Date();

$(function () {

    $('#current_month ').html(monthNames[date.getMonth()] + " " + date.getUTCFullYear());

    // ON CLICK SUR LEARN MORE
    $('#calendar').on("click", function (event) {
        event.preventDefault();
        $.ajax({
            type: 'POST',
            url: "/Horaire",
            data: {
                action: "getSeries"
            },
            success: function (resp) {

                console.log(resp);

                if (resp !== null) {
                    var options;
                    for (var i in resp) {
                        options += ('<option id="' + resp[i] + '">' + resp[i]);
                    }
                    $('#horaires ').html(options);
                    $('#btn_learn_more').css('display', 'none');
                    $('#horaires').css('display', 'inline-block');
                    $("#horaires").trigger('change');
                } else {
                    console.log(resp);
                }
            }
        });
    });

    $('#horaires').on('change', function (event) {
        $.ajax({
            url: '/Horaire',
            type: 'POST',
            data: {
                action: "getHoraire",
                horaire: $('#horaires option:checked').text()
            },
            success: function (resp) {
                console.log(resp);

                var resp_t = null;
                for(var i=0; i<resp.length; i++){
                    if(resp[i] != null){
                        resp_t = resp[i];
                    }
                }

                resp = resp_t;
                var header = '<div class="calendar__header"><div>mon</div><div>tue</div><div>wed</div><div>thu</div><div>fri</div></div>'
                var days = "";
                var indice = 1;

                for (var i=0; i<4; i++) {
                    var div = '<div class="calendar__week">';
                    var day = "";
                    for(var j=0; j<5; j++){
                        day += ('<div class="calendar__day day">' + indice + '<div class="horaire">' + '<br/>' + resp[indice - 1][0] + " - " + resp[indice - 1][1] + " - " + resp[indice - 1][2] + '</div>' + '</div>');
                        indice ++;
                    }
                    days += div + day + '</div>';
                }

                $('#cal ').html(header + days);
            }
        })
    });

});


function FormToJSON(racine) {
    var o = {};

    racine.find('input').each(function (i, el) {
        el = $(el);
        o[el.attr('name')] = el.val();
    });

    return JSON.stringify(o);
}

function JSONToForm(racine, json) {

    //json = JSON.parse(json);

    for (var key in json) {

        var item = json[key];
        var el = racine.find('[name="' + key + '"]');

        el.val(item);
    }
}