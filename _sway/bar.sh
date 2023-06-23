date_formatted=$(date "+%a %d-%b, %Y")
time=$(date "+%T")
battery_charge=$(upower --show-info $(upower --enumerate | grep 'BAT') | egrep "percentage" | awk '{print $2}')
battery_plug=$(upower --show-info $(upower --enumerate | grep 'BAT') | egrep "state" | awk '{print $2}')
network=$(ip route get 1.1.1.1 | grep -Po '(?<=dev\s)\w+' | cut -f1 -d ' ')
language=$(swaymsg -r -t get_inputs | awk '/1:1:AT_Translated_Set_2_keyboard/;/xkb_active_layout_name/' | grep -A1 '\b1:1:AT_Translated_Set_2_keyboard\b' | grep "xkb_active_layout_name" | awk -F '"' '{print $4}')
audio_muted=$(pulseaudio-ctl full-status |  awk -F ' ' '{print $2}')
audio_level=$(pulseaudio-ctl full-status |  awk -F ' ' '{print $1}')


# batteries:        

if [ $battery_plug = "discharging" ];
then
    battery_pluggedin='⚠'
else
    battery_pluggedin='⚡'
fi

if ! [ $network ]
then
    network_active="↹"
else
    network_active="⇆"
fi

if [ "$language" = "English (US)" ];
then
    ln='EN'
elif [ "$language" = "Bulgarian (traditional phonetic)" ]
then
    ln='BG'
else
    ln="$language"
fi

if [ "$audio_muted" = "yes" ]
then
    audio_icon='🔇'
elif [ "$audio_muted" = "no" ]
then
    audio_icon='🔊'
fi

echo "$audio_level $audio_icon | $ln | $network_active $network | $battery_pluggedin $battery_charge  | $date_formatted 🕘 $time"
