*.
*   CropMarks - draw crop marks around the graphics window
*
*	04/20/89 13:34
*..
	subroutine CropMarks
	include 'eosinclude:display.inc'
      character*100 SccsFileID
     -/'@(#)cropmarks.for	5.1 98/01/08 APS/ASF\0'/

	integer dlx,dly

	call GrafComment('Crop marks')

	dlx = min(swidex,swidey) / 30
	dly = dlx
	dlx = sign(dlx,swidex) * DevScaleX
	dly = sign(dly,swidey) * DevScaleY

	call line(screenx(1),screeny(1),screenx(1)+dlx,screeny(1))
	call line(screenx(1),screeny(1),screenx(1),screeny(1)+dly)
	call line(screenx(2)-dlx,screeny(1),screenx(2),screeny(1))
	call line(screenx(2),screeny(1),screenx(2),screeny(1)+dly)
	call line(screenx(1),screeny(2),screenx(1)+dlx,screeny(2))
	call line(screenx(1),screeny(2),screenx(1),screeny(2)-dly)
	call line(screenx(2)-dlx,screeny(2),screenx(2),screeny(2))
	call line(screenx(2),screeny(2),screenx(2),screeny(2)-dly)

	end
