#!/bin/csh -f

#  To run the command should be "new_uuid.csh < sendtoFA.template"
#     you must send it the template file
#  The output will be "sendtoFA.idl"
#    written by Samah Sohrab on 9/11/96

@ more = 1

/bin/rm sendtoFA.idl

while ($more == 1)

    set line = $<

    echo "$line" | grep "uuid(" >&! /dev/null

    if ($status == 0) then

       set new_uuid = `/bin/uuidgen`
       set first_part = `echo "$line" | cut -c 1-5`
       set last_part = `echo "$line" | cut -c 42-50`
       set output_line = $first_part$new_uuid$last_part
       echo $output_line >> sendtoFA.idl

    else

       echo "$line" >> sendtoFA.idl

    endif

    echo "$line" | grep " End of File " >&! /dev/null

    @ more = $status

end

exit 0
