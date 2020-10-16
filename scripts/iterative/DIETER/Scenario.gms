
*====================================================================================================
$ontext
The Dispatch and Investment Evaluation Tool with Endogenous Renewables (DIETER).
Version 1.0.2, January 2016.
Written by Alexander Zerrahn and Wolf-Peter Schill.
This work is licensed under the MIT License (MIT).
For more information on this license, visit http://opensource.org/licenses/mit-license.php.
Whenever you use this code, please refer to http://www.diw.de/dieter.
This version constitutes a minor revision of the model documented in Zerrahn, A., Schill, W.-P. (2015): A greenfield model to evaluate long-run power storage requirements for high shares of renewables. DIW Discussion Paper 1457. http://www.diw.de/documents/publikationen/73/diw_01.c.498475.de/dp1457.pdf
We are happy to receive feedback under azerrahn@diw.de and wschill@diw.de.
$offtext
*====================================================================================================

*==================================================*
*===== Scenario file                   =====
*===== only restrictions on parameters =====
*==================================================*
*Set reg        /DEU,IND/;

*set reg_solve(reg);
*reg_solve("DEU")=NO;
*reg_solve("IND")=YES;

* For example, PV costs decrease by 50%
*c_ri('Solar') = 0.5 * c_ri('Solar') ;

Set share / 0*150 / ;

*######################################
Set loop_phi_spv(share) /15/;
*Set loop_phi_spv(share) /50/;

Parameter phi_spv(share);
phi_spv(loop_phi_spv) = loop_phi_spv.val/100;

*######################################
Set loop_phi_wind_on(share) /15/;
*Set loop_phi_wind_on(share) /1,10,20,30,40,50,60/;

Parameter phi_wind_on(share);
phi_wind_on(loop_phi_wind_on) = loop_phi_wind_on.val/100;


*######################################
Set loop_phi_wind_off(share) /0/ ;

Parameter phi_wind_off(share);
phi_wind_off(loop_phi_wind_off) = loop_phi_wind_off.val/100;

*######################################
* power to gas share
Set loop_phi_p2g(share) / 0/ ;

Parameter phi_p2g(share);
phi_p2g(loop_phi_p2g) = loop_phi_p2g.val/100;


alias(share, share1, share2, share3);

parameter spv_share, wind_share_on, wind_share_off, p2g_share;





