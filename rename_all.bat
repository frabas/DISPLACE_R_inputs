@echo off
setlocal enabledelayedexpansion






set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_gscale_cpue_per_stk_on_nodes_quarter1.dat') do (
 ren %%a DEN!count!_gscale_cpue_per_stk_on_nodes_quarter1.dat
REM timeout /t 10

@echo DEN!count!  %%a >> names.txt

 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_gscale_cpue_per_stk_on_nodes_quarter2.dat') do (
 ren %%a DEN!count!_gscale_cpue_per_stk_on_nodes_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_gscale_cpue_per_stk_on_nodes_quarter3.dat') do (
 ren %%a DEN!count!_gscale_cpue_per_stk_on_nodes_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_gscale_cpue_per_stk_on_nodes_quarter4.dat') do (
 ren %%a DEN!count!_gscale_cpue_per_stk_on_nodes_quarter4.dat
REM timeout /t 10
 set /a count+=1
)



set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_gshape_cpue_per_stk_on_nodes_quarter1.dat') do (
 ren %%a DEN!count!_gshape_cpue_per_stk_on_nodes_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_gshape_cpue_per_stk_on_nodes_quarter2.dat') do (
 ren %%a DEN!count!_gshape_cpue_per_stk_on_nodes_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_gshape_cpue_per_stk_on_nodes_quarter3.dat') do (
 ren %%a DEN!count!_gshape_cpue_per_stk_on_nodes_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_gshape_cpue_per_stk_on_nodes_quarter4.dat') do (
 ren %%a DEN!count!_gshape_cpue_per_stk_on_nodes_quarter4.dat
REM timeout /t 10
 set /a count+=1
)


set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_cpue_per_stk_on_nodes_quarter1.dat') do (
 ren %%a DEN!count!_cpue_per_stk_on_nodes_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_cpue_per_stk_on_nodes_quarter2.dat') do (
 ren %%a DEN!count!_cpue_per_stk_on_nodes_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_cpue_per_stk_on_nodes_quarter3.dat') do (
 ren %%a DEN!count!_cpue_per_stk_on_nodes_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_cpue_per_stk_on_nodes_quarter4.dat') do (
 ren %%a DEN!count!_cpue_per_stk_on_nodes_quarter4.dat
REM timeout /t 10
 set /a count+=1
)






set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_freq_possible_metiers_quarter1.dat') do (
 ren %%a DEN!count!_freq_possible_metiers_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_freq_possible_metiers_quarter2.dat') do (
 ren %%a DEN!count!_freq_possible_metiers_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_freq_possible_metiers_quarter3.dat') do (
 ren %%a DEN!count!_freq_possible_metiers_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_freq_possible_metiers_quarter4.dat') do (
 ren %%a DEN!count!_freq_possible_metiers_quarter4.dat
REM timeout /t 10
 set /a count+=1
)




set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_possible_metiers_quarter1.dat') do (
ren %%a DEN!count!_possible_metiers_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_possible_metiers_quarter2.dat') do (
 ren %%a DEN!count!_possible_metiers_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_possible_metiers_quarter3.dat') do (
 ren %%a DEN!count!_possible_metiers_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DNK*_possible_metiers_quarter4.dat') do (
 ren %%a DEN!count!_possible_metiers_quarter4.dat
REM timeout /t 10
 set /a count+=1
)





REM DEU



set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_gscale_cpue_per_stk_on_nodes_quarter1.dat') do (
 ren %%a GER!count!_gscale_cpue_per_stk_on_nodes_quarter1.dat
REM timeout /t 10
@echo GER!count!  %%a >> names.txt
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_gscale_cpue_per_stk_on_nodes_quarter2.dat') do (
 ren %%a GER!count!_gscale_cpue_per_stk_on_nodes_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_gscale_cpue_per_stk_on_nodes_quarter3.dat') do (
 ren %%a GER!count!_gscale_cpue_per_stk_on_nodes_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_gscale_cpue_per_stk_on_nodes_quarter4.dat') do (
 ren %%a GER!count!_gscale_cpue_per_stk_on_nodes_quarter4.dat
REM timeout /t 10
 set /a count+=1
)



set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_gshape_cpue_per_stk_on_nodes_quarter1.dat') do (
 ren %%a GER!count!_gshape_cpue_per_stk_on_nodes_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_gshape_cpue_per_stk_on_nodes_quarter2.dat') do (
 ren %%a GER!count!_gshape_cpue_per_stk_on_nodes_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_gshape_cpue_per_stk_on_nodes_quarter3.dat') do (
 ren %%a GER!count!_gshape_cpue_per_stk_on_nodes_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_gshape_cpue_per_stk_on_nodes_quarter4.dat') do (
 ren %%a GER!count!_gshape_cpue_per_stk_on_nodes_quarter4.dat
REM timeout /t 10
 set /a count+=1
)


set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_cpue_per_stk_on_nodes_quarter1.dat') do (
 ren %%a GER!count!_cpue_per_stk_on_nodes_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_cpue_per_stk_on_nodes_quarter2.dat') do (
 ren %%a GER!count!_cpue_per_stk_on_nodes_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_cpue_per_stk_on_nodes_quarter3.dat') do (
 ren %%a GER!count!_cpue_per_stk_on_nodes_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_cpue_per_stk_on_nodes_quarter4.dat') do (
 ren %%a GER!count!_cpue_per_stk_on_nodes_quarter4.dat
REM timeout /t 10
 set /a count+=1
)





set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_freq_possible_metiers_quarter1.dat') do (
 ren %%a GER!count!_freq_possible_metiers_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_freq_possible_metiers_quarter2.dat') do (
 ren %%a GER!count!_freq_possible_metiers_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_freq_possible_metiers_quarter3.dat') do (
 ren %%a GER!count!_freq_possible_metiers_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_freq_possible_metiers_quarter4.dat') do (
 ren %%a GER!count!_freq_possible_metiers_quarter4.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_possible_metiers_quarter1.dat') do (
 ren %%a GER!count!_possible_metiers_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_possible_metiers_quarter2.dat') do (
 ren %%a GER!count!_possible_metiers_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_possible_metiers_quarter3.dat') do (
 ren %%a GER!count!_possible_metiers_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o DEU*_possible_metiers_quarter4.dat') do (
 ren %%a GER!count!_possible_metiers_quarter4.dat
REM timeout /t 10
 set /a count+=1
)



REM SWE



set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_gscale_cpue_per_stk_on_nodes_quarter1.dat') do (
 ren %%a SWN!count!_gscale_cpue_per_stk_on_nodes_quarter1.dat
REM timeout /t 10
@echo SWN!count!  %%a >> names.txt
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_gscale_cpue_per_stk_on_nodes_quarter2.dat') do (
 ren %%a SWN!count!_gscale_cpue_per_stk_on_nodes_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_gscale_cpue_per_stk_on_nodes_quarter3.dat') do (
 ren %%a SWN!count!_gscale_cpue_per_stk_on_nodes_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_gscale_cpue_per_stk_on_nodes_quarter4.dat') do (
 ren %%a SWN!count!_gscale_cpue_per_stk_on_nodes_quarter4.dat
REM timeout /t 10
 set /a count+=1
)



set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_gshape_cpue_per_stk_on_nodes_quarter1.dat') do (
 ren %%a SWN!count!_gshape_cpue_per_stk_on_nodes_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_gshape_cpue_per_stk_on_nodes_quarter2.dat') do (
 ren %%a SWN!count!_gshape_cpue_per_stk_on_nodes_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_gshape_cpue_per_stk_on_nodes_quarter3.dat') do (
 ren %%a SWN!count!_gshape_cpue_per_stk_on_nodes_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_gshape_cpue_per_stk_on_nodes_quarter4.dat') do (
 ren %%a SWN!count!_gshape_cpue_per_stk_on_nodes_quarter4.dat
REM timeout /t 10
 set /a count+=1
)


set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_cpue_per_stk_on_nodes_quarter1.dat') do (
 ren %%a SWN!count!_cpue_per_stk_on_nodes_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_cpue_per_stk_on_nodes_quarter2.dat') do (
 ren %%a SWN!count!_cpue_per_stk_on_nodes_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_cpue_per_stk_on_nodes_quarter3.dat') do (
 ren %%a SWN!count!_cpue_per_stk_on_nodes_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_cpue_per_stk_on_nodes_quarter4.dat') do (
 ren %%a SWN!count!_cpue_per_stk_on_nodes_quarter4.dat
REM timeout /t 10
 set /a count+=1
)





set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_freq_possible_metiers_quarter1.dat') do (
 ren %%a SWN!count!_freq_possible_metiers_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_freq_possible_metiers_quarter2.dat') do (
 ren %%a SWN!count!_freq_possible_metiers_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_freq_possible_metiers_quarter3.dat') do (
 ren %%a SWN!count!_freq_possible_metiers_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_freq_possible_metiers_quarter4.dat') do (
 ren %%a SWN!count!_freq_possible_metiers_quarter4.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_possible_metiers_quarter1.dat') do (
 ren %%a SWN!count!_possible_metiers_quarter1.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_possible_metiers_quarter2.dat') do (
 ren %%a SWN!count!_possible_metiers_quarter2.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_possible_metiers_quarter3.dat') do (
 ren %%a SWN!count!_possible_metiers_quarter3.dat
REM timeout /t 10
 set /a count+=1
)

set /a count=0
for /f "tokens=*" %%a in ('dir /b /o SWE*_possible_metiers_quarter4.dat') do (
 ren %%a SWN!count!_possible_metiers_quarter4.dat
REM timeout /t 10
 set /a count+=1
)





