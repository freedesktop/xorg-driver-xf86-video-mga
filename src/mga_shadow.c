/*
   Copyright (c) 1999,  The XFree86 Project Inc. 
   Written by Mark Vojkovich <markv@valinux.com>
*/

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "xf86.h"
#include "xf86_OSproc.h"
#include "xf86Pci.h"
#include "mga_reg.h"
#include "mga.h"
#include "shadowfb.h"
#include "servermd.h"

void
MGARefreshArea(ScrnInfoPtr pScrn, int num, BoxPtr pbox)
{
    MGAPtr pMga = MGAPTR(pScrn);
    int width, height, Bpp, FBPitch;
    unsigned char *src, *dst;
   
    Bpp = pScrn->bitsPerPixel >> 3;
    if (pMga->randr12)
	FBPitch = BitmapBytePad(pScrn->virtualX * pScrn->bitsPerPixel);
    else
	FBPitch = BitmapBytePad(pScrn->displayWidth * pScrn->bitsPerPixel);

    while(num--) {
	/* clip the box to the screen */
	pbox->x1 = max(pbox->x1, 0);
	pbox->y1 = max(pbox->y1, 0);
	pbox->x2 = min(pbox->x2, pScrn->pScreen->width - 1);
	pbox->y2 = min(pbox->y2, pScrn->pScreen->height - 1);

	width = (pbox->x2 - pbox->x1) * Bpp;
	height = pbox->y2 - pbox->y1;

	if (height < 1 || width < 1)
	    continue;

	src = pMga->ShadowPtr + (pbox->y1 * pMga->ShadowPitch) + 
						(pbox->x1 * Bpp);
	dst = pMga->FbStart + (pbox->y1 * FBPitch) + (pbox->x1 * Bpp);

	while (height--) {
	    memcpy(dst, src, width);
	    dst += FBPitch;
	    src += pMga->ShadowPitch;
	}
	
	pbox++;
    }
} 

void
MGAPointerMoved(SCRN_ARG_TYPE arg, int x, int y)
{
    SCRN_INFO_PTR(arg);
    MGAPtr pMga = MGAPTR(pScrn);
    int newX, newY;

    newX = y;
    newY = pScrn->pScreen->width - x - 1;

    (*pMga->PointerMoved)(arg, newX, newY);
}
