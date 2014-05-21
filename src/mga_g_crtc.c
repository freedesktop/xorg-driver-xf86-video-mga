/*
 * Copyright 2007 Tilman Sauerbeck
 * Copyright 2012 Red Hat, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software")
 * to deal in the software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * them Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTIBILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES, OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT, OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Authors:
 *    Tilman Sauerbeck <tilman@code-monkey.de>
 *    Adam Jackson <ajax@redhat.com>
 *
 * Sources:
 *    xf86-video-intel, mga_dacG.c
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

/* All drivers should typically include these */
#include "xf86.h"
#include "xf86_OSproc.h"

/* Drivers that need to access the PCI config space directly need this */
#include "xf86Pci.h"

#include "mga_reg.h"
#include "mga.h"
#include "mga_macros.h"
#include "mga_randr.h"

#include <unistd.h>

/*
 * Only change bits shown in this mask.  Ideally reserved bits should be
 * zeroed here.  Also, don't change the vgaioen bit here since it is
 * controlled elsewhere.
 *
 * XXX These settings need to be checked.
 */
#define OPTION1_MASK	0xFFFFFEFF
#define OPTION2_MASK	0xFFFFFFFF
#define OPTION3_MASK	0xFFFFFFFF

#define OPTION1_MASK_PRIMARY	0xFFFC0FF

typedef struct {
    unsigned char ExtVga[6];
    unsigned char DacRegs[0x50];
    unsigned char PllM;
    unsigned char PllN;
    unsigned char PllP;
    unsigned char ExtVga_Index24;
    unsigned char Dac_Index90;
    CARD32 Option;
    CARD32 Option2;
    CARD32 Option3;
    Bool PIXPLLCSaved;
    long clock;
} MgaCrtcStateRec, *MgaCrtcStatePtr;

typedef struct {
    MgaCrtcStateRec saved_state;
} MgaCrtcDataRec, *MgaCrtcDataPtr;

static void
MGAG200WBPrepareForModeSwitch(ScrnInfoPtr pScrn)
{
    MGAPtr pMga = MGAPTR(pScrn);

    unsigned char ucTmpData = 0;
    int ulIterationMax = 0;
    // 1- The first step is to warn the BMC of an upcoming mode change.
    // We are putting the misc<0> to output.
    ucTmpData = inMGAdac(MGA1064_GEN_IO_CTL);
    ucTmpData |= 0x10;
    outMGAdac(MGA1064_GEN_IO_CTL, ucTmpData);

    // We are putting a 1 on the misc<0> line.
    ucTmpData = inMGAdac(MGA1064_GEN_IO_DATA);
    ucTmpData |= 0x10;
    outMGAdac(MGA1064_GEN_IO_DATA, ucTmpData);

    // 2- The second step is to mask any further scan request
    // This will be done by asserting the remfreqmsk bit (XSPAREREG<7>)
    ucTmpData = inMGAdac(MGA1064_SPAREREG);
    ucTmpData |= 0x80;
    outMGAdac(MGA1064_SPAREREG, ucTmpData);

    // 3a- The third step is to verify if there is an active scan
    // We are searching for a 0 on remhsyncsts (XSPAREREG<0>)
    ulIterationMax = 300;
    while (!(ucTmpData & 0x01) && ulIterationMax)
    {
        ucTmpData = inMGAdac(MGA1064_SPAREREG);
        usleep(1000);
        ulIterationMax--;
    }

    // 3b- This step occurs only if the remote is actually scanning
    // We are waiting for the end of the frame which is a 1 on
    // remvsyncsts (XSPAREREG<1>)
    if (ulIterationMax)
    {
        ulIterationMax = 300;
        while ((ucTmpData & 0x02) && ulIterationMax)
        {
            ucTmpData = inMGAdac(MGA1064_SPAREREG);
            usleep(1000);
            ulIterationMax--;
        }
    }
}

static void
MGAG200WBRestoreFromModeSwitch(ScrnInfoPtr pScrn)
{
    MGAPtr pMga = MGAPTR(pScrn);

    unsigned char ucTmpData = 0;

    // 1- The first step is to ensure that the vrsten and hrsten are set
    OUTREG8(MGAREG_CRTCEXT_INDEX, 0x01);
    ucTmpData = INREG8(MGAREG_CRTCEXT_DATA);
    OUTREG8(MGAREG_CRTCEXT_DATA, ucTmpData | 0x88);

    // 2- The second step is is to assert the rstlvl2
    ucTmpData = inMGAdac(MGA1064_REMHEADCTL2);
    ucTmpData |= 0x08;
    outMGAdac(MGA1064_REMHEADCTL2, ucTmpData);

    // - Wait for 10 us
    usleep(10);

    // 3- The next step is is to deassert the rstlvl2
    ucTmpData &= ~0x08;
    outMGAdac(MGA1064_REMHEADCTL2, ucTmpData);

    // - Wait for 10 us
    usleep(10);

    // 4- The fourth step is to remove the mask of scan request
    // This will be done by deasserting the remfreqmsk bit (XSPAREREG<7>)
    ucTmpData = inMGAdac(MGA1064_SPAREREG);
    ucTmpData &= ~0x80;
    outMGAdac(MGA1064_SPAREREG, ucTmpData);

    // 5- Finally, we are putting back a 0 on the misc<0> line.
    ucTmpData = inMGAdac(MGA1064_GEN_IO_DATA);
    ucTmpData &= ~0x10;
    outMGAdac(MGA1064_GEN_IO_DATA, ucTmpData);
}

static void
MGAG200EVPIXPLLSET(ScrnInfoPtr pScrn, MgaCrtcStatePtr mgaReg)
{
    MGAPtr pMga = MGAPTR(pScrn);

    unsigned char ucTempByte, ucPixCtrl;

    // Set pixclkdis to 1
    ucPixCtrl = inMGAdac(MGA1064_PIX_CLK_CTL);
    ucPixCtrl |= MGA1064_PIX_CLK_CTL_CLK_DIS;
    outMGAdac(MGA1064_PIX_CLK_CTL, ucPixCtrl);

    // Select PLL Set C
    ucTempByte = INREG8(MGAREG_MEM_MISC_READ);
    ucTempByte |= 0x3<<2; //select MGA pixel clock
    OUTREG8(MGAREG_MEM_MISC_WRITE, ucTempByte);

    // Set pixlock to 0
    ucTempByte = inMGAdac(MGA1064_PIX_PLL_STAT);
    outMGAdac(MGA1064_PIX_PLL_STAT, ucTempByte & ~0x40);

    //    Set pix_stby to 1
    ucPixCtrl |= MGA1064_PIX_CLK_CTL_CLK_POW_DOWN;
    outMGAdac(MGA1064_PIX_CLK_CTL, ucPixCtrl);

    // Program the Pixel PLL Register
    outMGAdac(MGA1064_EV_PIX_PLLC_M, mgaReg->PllM);
    outMGAdac(MGA1064_EV_PIX_PLLC_N, mgaReg->PllN);
    outMGAdac(MGA1064_EV_PIX_PLLC_P, mgaReg->PllP);

    // Wait 50 us
    usleep(50);

    // Set pix_stby to 0
    ucPixCtrl &= ~MGA1064_PIX_CLK_CTL_CLK_POW_DOWN;
    outMGAdac(MGA1064_PIX_CLK_CTL, ucPixCtrl);

    // Wait 500 us
    usleep(500);

    // Select the pixel PLL by setting pixclksel to 1
    ucTempByte = inMGAdac(MGA1064_PIX_CLK_CTL);
    ucTempByte &= ~MGA1064_PIX_CLK_CTL_SEL_MSK;
    ucTempByte |= MGA1064_PIX_CLK_CTL_SEL_PLL;
    outMGAdac(MGA1064_PIX_CLK_CTL, ucTempByte);

    // Set pixlock to 1
    ucTempByte = inMGAdac(MGA1064_PIX_PLL_STAT);
    outMGAdac(MGA1064_PIX_CLK_CTL, ucTempByte | 0x40);

    // Reset dotclock rate bit.
    ucTempByte = INREG8(MGAREG_MEM_MISC_READ);
    ucTempByte |= 0x3<<2; //select MGA pixel clock
    OUTREG8(MGAREG_MEM_MISC_WRITE, ucTempByte);

    OUTREG8(MGAREG_SEQ_INDEX, 1);
    ucTempByte = INREG8(MGAREG_SEQ_DATA);
    OUTREG8(MGAREG_SEQ_DATA, ucTempByte & ~0x8);

    // Set pixclkdis to 0
    ucTempByte = inMGAdac(MGA1064_PIX_CLK_CTL);
    ucTempByte &= ~MGA1064_PIX_CLK_CTL_CLK_DIS;
    outMGAdac(MGA1064_PIX_CLK_CTL, ucTempByte);
}

static void
MGAG200WBPIXPLLSET(ScrnInfoPtr pScrn, MgaCrtcStatePtr mgaReg)
{
    MGAPtr pMga = MGAPTR(pScrn);

    unsigned long ulLoopCount, ulLockCheckIterations = 0, ulTempCount, ulVCount;
    unsigned char ucTempByte, ucPixCtrl, ucPLLLocked = FALSE;

    while(ulLockCheckIterations <= 32 && ucPLLLocked == FALSE)
    {
        if(ulLockCheckIterations > 0)
        {
            OUTREG8(MGAREG_CRTCEXT_INDEX, 0x1E);
            ucTempByte = INREG8(MGAREG_CRTCEXT_DATA);
            if(ucTempByte < 0xFF)
            {
                OUTREG8(MGAREG_CRTCEXT_DATA, ucTempByte+1);
            }
        }

        // Set pixclkdis to 1
        ucPixCtrl = inMGAdac(MGA1064_PIX_CLK_CTL);
        ucPixCtrl |= MGA1064_PIX_CLK_CTL_CLK_DIS;
        outMGAdac(MGA1064_PIX_CLK_CTL, ucPixCtrl);

        ucTempByte = inMGAdac(MGA1064_REMHEADCTL);
        ucTempByte |= MGA1064_REMHEADCTL_CLKDIS;
        outMGAdac(MGA1064_REMHEADCTL, ucTempByte);

        // Select PLL Set C
        ucTempByte = INREG8(MGAREG_MEM_MISC_READ);
        ucTempByte |= 0x3<<2; //select MGA pixel clock
        OUTREG8(MGAREG_MEM_MISC_WRITE, ucTempByte);

        ucPixCtrl |= MGA1064_PIX_CLK_CTL_CLK_POW_DOWN | 0x80;
        outMGAdac(MGA1064_PIX_CLK_CTL, ucPixCtrl);

        // Wait 500 us
        usleep(500);

        // Reset the PLL
        //   When we are varying the output frequency by more than
        //   10%, we must reset the PLL. However to be prudent, we
        //   will reset it each time that we are changing it.
        ucTempByte = inMGAdac(MGA1064_VREF_CTL);
        ucTempByte &= ~0x04;
        outMGAdac(MGA1064_VREF_CTL, ucTempByte );

        // Wait 50 us
        usleep(50);

        // Program the Pixel PLL Register
        outMGAdac(MGA1064_WB_PIX_PLLC_N, mgaReg->PllN);
        outMGAdac(MGA1064_WB_PIX_PLLC_M, mgaReg->PllM);
        outMGAdac(MGA1064_WB_PIX_PLLC_P, mgaReg->PllP);

        // Wait 50 us
        usleep(50);

        // Turning the PLL on
        ucTempByte = inMGAdac(MGA1064_VREF_CTL);
        ucTempByte |= 0x04;
        outMGAdac(MGA1064_VREF_CTL, ucTempByte );

        // Wait 500 us
        usleep(500);

        // Select the pixel PLL by setting pixclksel to 1
        ucTempByte = inMGAdac(MGA1064_PIX_CLK_CTL);
        ucTempByte &= ~MGA1064_PIX_CLK_CTL_SEL_MSK;
        ucTempByte |= MGA1064_PIX_CLK_CTL_SEL_PLL;
        outMGAdac(MGA1064_PIX_CLK_CTL, ucTempByte);

        ucTempByte = inMGAdac(MGA1064_REMHEADCTL);
        ucTempByte &= ~MGA1064_REMHEADCTL_CLKSL_MSK;
        ucTempByte |= MGA1064_REMHEADCTL_CLKSL_PLL;
        outMGAdac(MGA1064_REMHEADCTL, ucTempByte);

        // Reset dotclock rate bit.
        OUTREG8(MGAREG_SEQ_INDEX, 1);
        ucTempByte = INREG8(MGAREG_SEQ_DATA);
        OUTREG8(MGAREG_SEQ_DATA, ucTempByte & ~0x8);

        // Set pixclkdis to 0
        ucTempByte = inMGAdac(MGA1064_PIX_CLK_CTL);
        ucTempByte &= ~MGA1064_PIX_CLK_CTL_CLK_DIS;
        outMGAdac(MGA1064_PIX_CLK_CTL, ucTempByte);

        // Poll VCount. If it increments twice inside 150us,
        // we assume that the PLL has locked.
        ulLoopCount = 0;
        ulVCount = INREG(MGAREG_VCOUNT);

        while(ulLoopCount < 30 && ucPLLLocked == FALSE)
        {
            ulTempCount = INREG(MGAREG_VCOUNT);

            if(ulTempCount < ulVCount)
            {
                ulVCount = 0;
            }
            if ((ucTempByte - ulVCount) > 2)
            {
                ucPLLLocked = TRUE;
            }
            else
            {
                usleep(5);
            }
            ulLoopCount++;
        }
        ulLockCheckIterations++;
    }

    // Set remclkdis to 0
    ucTempByte = inMGAdac(MGA1064_REMHEADCTL);
    ucTempByte &= ~MGA1064_REMHEADCTL_CLKDIS;
    outMGAdac(MGA1064_REMHEADCTL, ucTempByte);
}

static void
MGAG200ERPIXPLLSET(ScrnInfoPtr pScrn, MgaCrtcStatePtr mgaReg)
{
    //TODO  G200ER Validate sequence
    CARD8 ucPixCtrl, ucTempByte;
    MGAPtr pMga = MGAPTR(pScrn);


    // Set pixclkdis to 1
    ucPixCtrl = inMGAdac(MGA1064_PIX_CLK_CTL);
    ucPixCtrl |= MGA1064_PIX_CLK_CTL_CLK_DIS;
    outMGAdac(MGA1064_PIX_CLK_CTL, ucPixCtrl);

    ucTempByte = inMGAdac(MGA1064_REMHEADCTL);
    ucTempByte |= MGA1064_REMHEADCTL_CLKDIS;
    outMGAdac(MGA1064_REMHEADCTL, ucTempByte);

    // Select PLL Set C
    ucTempByte = INREG8(MGAREG_MEM_MISC_READ);
    ucTempByte |= (0x3<<2) | 0xc0; //select MGA pixel clock
    OUTREG8(MGAREG_MEM_MISC_WRITE, ucTempByte);

    ucPixCtrl &= ~MGA1064_PIX_CLK_CTL_CLK_DIS;
    ucPixCtrl |= MGA1064_PIX_CLK_CTL_CLK_POW_DOWN;
    outMGAdac(MGA1064_PIX_CLK_CTL, ucPixCtrl);

    // Wait 500 us
    usleep(500);

    // Program the Pixel PLL Register
    outMGAdac(MGA1064_ER_PIX_PLLC_N, mgaReg->PllN);
    outMGAdac(MGA1064_ER_PIX_PLLC_M, mgaReg->PllM);
    outMGAdac(MGA1064_ER_PIX_PLLC_P, mgaReg->PllP);

        // Wait 50 us
    usleep(50);

}

static void
MGAG200EHPIXPLLSET(ScrnInfoPtr pScrn, MgaCrtcStatePtr mgaReg)
{
    MGAPtr pMga = MGAPTR(pScrn);

    unsigned long ulFallBackCounter, ulLoopCount, ulLockCheckIterations = 0, ulTempCount, ulVCount;
    unsigned char ucTempByte, ucPixCtrl, ucPLLLocked = FALSE;
    unsigned char ucM;
    unsigned char ucN;
    unsigned char ucP;
    unsigned char ucS;

    while(ulLockCheckIterations <= 32 && ucPLLLocked == FALSE)
    {
        // Set pixclkdis to 1
        ucPixCtrl = inMGAdac(MGA1064_PIX_CLK_CTL);
        ucPixCtrl |= MGA1064_PIX_CLK_CTL_CLK_DIS;
        outMGAdac(MGA1064_PIX_CLK_CTL, ucPixCtrl);

        // Select PLL Set C
        ucTempByte = INREG8(MGAREG_MEM_MISC_READ);
        ucTempByte |= 0x3<<2; //select MGA pixel clock
        OUTREG8(MGAREG_MEM_MISC_WRITE, ucTempByte);

        ucPixCtrl |= MGA1064_PIX_CLK_CTL_CLK_POW_DOWN;
        ucPixCtrl &= ~0x80;
        outMGAdac(MGA1064_PIX_CLK_CTL, ucPixCtrl);

        // Wait 500 us
        usleep(500);

        // Program the Pixel PLL Register
        outMGAdac(MGA1064_EH_PIX_PLLC_N, mgaReg->PllN);
        outMGAdac(MGA1064_EH_PIX_PLLC_M, mgaReg->PllM);
        outMGAdac(MGA1064_EH_PIX_PLLC_P, mgaReg->PllP);

        // Wait 500 us
        usleep(500);

        // Select the pixel PLL by setting pixclksel to 1
        ucTempByte = inMGAdac(MGA1064_PIX_CLK_CTL);
        ucTempByte &= ~MGA1064_PIX_CLK_CTL_SEL_MSK;
        ucTempByte |= MGA1064_PIX_CLK_CTL_SEL_PLL;
        outMGAdac(MGA1064_PIX_CLK_CTL, ucTempByte);

        // Reset dotclock rate bit.
        OUTREG8(MGAREG_SEQ_INDEX, 1);
        ucTempByte = INREG8(MGAREG_SEQ_DATA);
        OUTREG8(MGAREG_SEQ_DATA, ucTempByte & ~0x8);

        // Set pixclkdis to 0 and pixplldn to 0
        ucTempByte = inMGAdac(MGA1064_PIX_CLK_CTL);
        ucTempByte &= ~MGA1064_PIX_CLK_CTL_CLK_DIS;
        ucTempByte &= ~MGA1064_PIX_CLK_CTL_CLK_POW_DOWN;
        outMGAdac(MGA1064_PIX_CLK_CTL, ucTempByte);

        // Poll VCount. If it increments twice inside 150us,
        // we assume that the PLL has locked.
        ulLoopCount = 0;
        ulVCount = INREG(MGAREG_VCOUNT);

        while(ulLoopCount < 30 && ucPLLLocked == FALSE)
        {
            ulTempCount = INREG(MGAREG_VCOUNT);

            if(ulTempCount < ulVCount)
            {
                ulVCount = 0;
            }
            if ((ucTempByte - ulVCount) > 2)
            {
                ucPLLLocked = TRUE;
            }
            else
            {
                usleep(5);
            }
            ulLoopCount++;
        }
        ulLockCheckIterations++;
    }
}


static void crtc_dpms(xf86CrtcPtr crtc, int mode);
static void crtc_save(xf86CrtcPtr crtc);
static void crtc_restore(xf86CrtcPtr crtc);
static Bool crtc_lock(xf86CrtcPtr crtc);
static void crtc_unlock(xf86CrtcPtr crtc);
static Bool crtc_mode_fixup(xf86CrtcPtr crtc, DisplayModePtr mode,
                            DisplayModePtr adjusted_mode);
static void crtc_prepare(xf86CrtcPtr crtc);
static void crtc_mode_set(xf86CrtcPtr crtc, DisplayModePtr mode,
                          DisplayModePtr adjusted_mode, int x, int y);
static void crtc_commit(xf86CrtcPtr crtc);
static void crtc_destroy(xf86CrtcPtr crtc);

static const xf86CrtcFuncsRec crtc_funcs = {
    .dpms = crtc_dpms,
    .save = crtc_save,
    .restore = crtc_restore,
    .lock = crtc_lock,
    .unlock = crtc_unlock,
    .mode_fixup = crtc_mode_fixup,
    .prepare = crtc_prepare,
    .mode_set = crtc_mode_set,
    .commit = crtc_commit,
    .destroy = crtc_destroy
};

static void
crtc_dpms(xf86CrtcPtr crtc, int mode)
{
}

/*
 * MGAGSetPCLK - Set the pixel (PCLK) clock.
 */
static void
MGAGSetPCLK(xf86CrtcPtr crtc, MgaCrtcStatePtr state, long f_out)
{
    MGAPtr pMga = MGAPTR(crtc->scrn);

    /* Pixel clock values */
    int m, n, p;

    state->clock = f_out;

    if (MGAISGx50(pMga)) {
        return;
    }

    if (pMga->is_G200SE) {
        MGAG200SEComputePLLParam(f_out, &m, &n, &p);
	/* the asymmetry here bugs me */
	state->DacRegs[MGA1064_PIX_PLLC_M] = m;
	state->DacRegs[MGA1064_PIX_PLLC_N] = n;
	state->DacRegs[MGA1064_PIX_PLLC_P] = p;
    } else {
	if (pMga->is_G200EV) {
	    MGAG200EVComputePLLParam(f_out, &m, &n, &p);
	} else if (pMga->is_G200WB) {
	    MGAG200WBComputePLLParam(f_out, &m, &n, &p);
	} else if (pMga->is_G200EH) {
	    MGAG200EHComputePLLParam(f_out, &m, &n, &p);
	} else if (pMga->is_G200ER) {
	    MGAG200ERComputePLLParam(f_out, &m, &n, &p);
	}

	state->PllM = m;
	state->PllN = n;
	state->PllP = p;
    }
}

static void
state_set(xf86CrtcPtr crtc, MgaCrtcStatePtr state,
          DisplayModePtr mode, int x, int y)
{
    /*
     * initial values of the DAC registers
     */
    const static unsigned char initDAC[] = {
        /* 0x00: */	   0,    0,    0,    0,    0,    0, 0x00,    0,
        /* 0x08: */	   0,    0,    0,    0,    0,    0,    0,    0,
        /* 0x10: */	   0,    0,    0,    0,    0,    0,    0,    0,
        /* 0x18: */	0x00,    0, 0xC9, 0xFF, 0xBF, 0x20, 0x1F, 0x20,
        /* 0x20: */	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        /* 0x28: */	0x00, 0x00, 0x00, 0x00,    0,    0,    0, 0x40,
        /* 0x30: */	0x00, 0xB0, 0x00, 0xC2, 0x34, 0x14, 0x02, 0x83,
        /* 0x38: */	0x00, 0x93, 0x00, 0x77, 0x00, 0x00, 0x00, 0x3A,
        /* 0x40: */	   0,    0,    0,    0,    0,    0,    0,    0,
        /* 0x48: */	   0,    0,    0,    0,    0,    0,    0,    0
    };

    int i;
    int hd, hs, he, ht, vd, vs, ve, vt, wd;
    int BppShift;
    MGAPtr pMga = MGAPTR(crtc->scrn);
    vgaRegPtr vga = &VGAHWPTR(crtc->scrn)->ModeReg;
    unsigned int startadd = (y * crtc->scrn->virtualX) + x;

    BppShift = pMga->BppShifts[(crtc->scrn->bitsPerPixel >> 3) - 1];

    for (i = 0; i < sizeof(state->DacRegs); i++)
        state->DacRegs[i] = initDAC[i];

    switch (pMga->Chipset) {
    case PCI_CHIP_MGAG200_SE_A_PCI:
    case PCI_CHIP_MGAG200_SE_B_PCI:
        state->DacRegs[MGA1064_VREF_CTL] = 0x03;
        state->DacRegs[MGA1064_PIX_CLK_CTL] =
            MGA1064_PIX_CLK_CTL_SEL_PLL;

        state->DacRegs[MGA1064_MISC_CTL] =
            MGA1064_MISC_CTL_DAC_EN |
            MGA1064_MISC_CTL_VGA8 |
            MGA1064_MISC_CTL_DAC_RAM_CS;

        if (pMga->HasSDRAM)
            state->Option = 0x40049120;

        state->Option2 = 0x00008000;
        break;

    case PCI_CHIP_MGAG200_WINBOND_PCI:
	state->DacRegs[MGA1064_VREF_CTL] = 0x07;
	state->Option = 0x41049120;
	state->Option2 = 0x0000b000;
	break;

    case PCI_CHIP_MGAG200_EV_PCI:
	state->DacRegs[MGA1064_PIX_CLK_CTL] =
	    MGA1064_PIX_CLK_CTL_SEL_PLL;

	state->DacRegs[MGA1064_MISC_CTL] =
	    MGA1064_MISC_CTL_VGA8 |
	    MGA1064_MISC_CTL_DAC_RAM_CS;

	state->Option = 0x00000120;
	state->Option2 = 0x0000b000;
	break;

    case PCI_CHIP_MGAG200_ER_PCI:
	state->Dac_Index90 = 0;
	break;

    case PCI_CHIP_MGAG200_EH_PCI:
	state->DacRegs[MGA1064_MISC_CTL] =
	    MGA1064_MISC_CTL_VGA8 |
	    MGA1064_MISC_CTL_DAC_RAM_CS;

	state->Option = 0x00000120;
	state->Option2 = 0x0000b000;
	break;

    default:
	xf86DrvMsg(crtc->scrn->scrnIndex, X_ERROR, "Oh dear\n");
        break;
    }

    /* must always have the pci retries on but rely on
       polling to keep them from occuring */
    state->Option &= ~0x20000000;

    switch (crtc->scrn->bitsPerPixel) {
    case 8:
        state->DacRegs[MGA1064_MUL_CTL] = MGA1064_MUL_CTL_8bits;
        startadd /= 8;
        break;
    case 16:
        state->DacRegs[MGA1064_MUL_CTL] = MGA1064_MUL_CTL_16bits;

        if ((crtc->scrn->weight.red == 5) && (crtc->scrn->weight.green == 5)
            && (crtc->scrn->weight.blue == 5)) {
            state->DacRegs[MGA1064_MUL_CTL] = MGA1064_MUL_CTL_15bits;
        }

        startadd /= 4;
        break;
    case 24:
        state->DacRegs[MGA1064_MUL_CTL] = MGA1064_MUL_CTL_24bits;
        startadd /= 8;
        break;
    case 32:
	state->DacRegs[MGA1064_MUL_CTL] = MGA1064_MUL_CTL_32_24bits;

        startadd /= 2;
        break;
    default:
        FatalError("MGA: unsupported depth\n");
    }

    /* we only have 20 bits to store the start address */
    startadd &= 0xfffff;

    /*
     * This will initialize all of the generic VGA registers.
     */
    if (!vgaHWInit(crtc->scrn, mode)) {
        ErrorF("oh noes, vgahwinit failed\n");
        return;
    }

    /*
     * Here all of the MGA registers get filled in.
     */
    hd = (mode->CrtcHDisplay >> 3) - 1;
    hs = (mode->CrtcHSyncStart >> 3) - 1;
    he = (mode->CrtcHSyncEnd >> 3) - 1;
    ht = (mode->CrtcHTotal >> 3) - 1;
    vd = mode->CrtcVDisplay - 1;
    vs = mode->CrtcVSyncStart - 1;
    ve = mode->CrtcVSyncEnd - 1;
    vt = mode->CrtcVTotal - 2;

    /* HTOTAL & 0x7 equal to 0x6 in 8bpp or 0x4 in 24bpp causes strange
     * vertical stripes
     */
    if ((ht & 0x07) == 0x06 || (ht & 0x07) == 0x04)
        ht++;

    if (crtc->scrn->bitsPerPixel == 24)
        wd = (mode->CrtcHDisplay * 3) >> (4 - BppShift);
    else
	wd = mode->CrtcHDisplay >> (4 - BppShift);

    state->ExtVga[0] = 0;
    state->ExtVga[5] = 0;

    if (mode->Flags & V_INTERLACE) {
        state->ExtVga[0] = 0x80;
        state->ExtVga[5] = (hs + he - ht) >> 1;
        wd <<= 1;
        vt &= 0xFFFE;
    }

    state->ExtVga[0] |= (wd & 0x300) >> 4;
    state->ExtVga[0] |= (startadd >> 16) & 0x0f;

    state->ExtVga[1] = (((ht - 4) & 0x100) >> 8) |
                       ((hd & 0x100) >> 7) |
                       ((hs & 0x100) >> 6) |
                       (ht & 0x40);
    state->ExtVga[2] = ((vt & 0xc00) >> 10) |
                       ((vd & 0x400) >> 8) |
                       ((vd & 0xc00) >> 7) |
                       ((vs & 0xc00) >> 5) |
                       ((vd & 0x400) >> 3); /* linecomp */

    if (crtc->scrn->bitsPerPixel == 24)
        state->ExtVga[3] = (((1 << BppShift) * 3) - 1) | 0x80;
    else
        state->ExtVga[3] = ((1 << BppShift) - 1) | 0x80;

    state->ExtVga[4] = 0;

    if (pMga->is_G200WB){
	state->ExtVga[1] |= 0x88;
    }
    state->ExtVga_Index24 = 0x05;

    vga->CRTC[0] = ht - 4;
    vga->CRTC[1] = hd;
    vga->CRTC[2] = hd;
    vga->CRTC[3] = (ht & 0x1F) | 0x80;
    vga->CRTC[4] = hs;
    vga->CRTC[5] = ((ht & 0x20) << 2) | (he & 0x1F);
    vga->CRTC[6] = vt & 0xFF;
    vga->CRTC[7] = ((vt & 0x100) >> 8 ) |
                   ((vd & 0x100) >> 7 ) |
                   ((vs & 0x100) >> 6 ) |
                   ((vd & 0x100) >> 5 ) |
                   ((vd & 0x100) >> 4 ) | /* linecomp */
                   ((vt & 0x200) >> 4 ) |
                   ((vd & 0x200) >> 3 ) |
                   ((vs & 0x200) >> 2 );
    vga->CRTC[9] = ((vd & 0x200) >> 4) |
                   ((vd & 0x200) >> 3); /* linecomp */

    vga->CRTC[12] = (startadd & 0xff00) >> 8;
    vga->CRTC[13] = startadd & 0xff;

    vga->CRTC[16] = vs & 0xFF;
    vga->CRTC[17] = (ve & 0x0F) | 0x20;
    vga->CRTC[18] = vd & 0xFF;
    vga->CRTC[19] = wd & 0xFF;
    vga->CRTC[21] = vd & 0xFF;
    vga->CRTC[22] = (vt + 1) & 0xFF;
    vga->CRTC[24] = vd & 0xFF; /* linecomp */

    state->DacRegs[MGA1064_CURSOR_BASE_ADR_LOW] = pMga->FbCursorOffset >> 10;
    state->DacRegs[MGA1064_CURSOR_BASE_ADR_HI] = pMga->FbCursorOffset >> 18;

    if (pMga->SyncOnGreen) {
        state->DacRegs[MGA1064_GEN_CTL] &=
            ~MGA1064_GEN_CTL_SYNC_ON_GREEN_DIS;

        state->ExtVga[3] |= 0x40;
    }

    /* select external clock */
    vga->MiscOutReg |= 0x0C;

    if (mode->Flags & V_DBLSCAN)
        vga->CRTC[9] |= 0x80;

    if (MGAISGx50(pMga)) {
        OUTREG(MGAREG_ZORG, 0);
    }

    MGAGSetPCLK(crtc, state, mode->Clock);

    /* This disables the VGA memory aperture */
    vga->MiscOutReg &= ~0x02;

#if X_BYTE_ORDER == X_BIG_ENDIAN
    /* Disable byte-swapping for big-endian architectures - the XFree
       driver seems to like a little-endian framebuffer -ReneR */
    /* state->Option |= 0x80000000; */
    state->Option &= ~0x80000000;
#endif
}

/*
 * This function restores a video mode. It basically writes out all of
 * the registers that have previously been saved in the MgaCrtcStateRec
 * data structure.
 */
static void
state_restore(xf86CrtcPtr crtc, MgaCrtcStatePtr state,
              vgaRegPtr vga, int vga_flags)
{
    ScrnInfoPtr scrn = crtc->scrn;
    MGAPtr pMga = MGAPTR(scrn);
    CARD32 optionMask;
    int i;

    if (pMga->is_G200WB)
	MGAG200WBPrepareForModeSwitch(scrn);

    /*
     * Pixel Clock needs to be restored regardless if we use
     * HALLib or not. HALlib doesn't do a good job restoring
     * VESA modes. MATROX: hint, hint.
     *
     * FIXME: This seems weird. Verify.
     */
    if (MGAISGx50(pMga) && state->clock) {
        MGAG450SetPLLFreq(scrn, state->clock);
        state->PIXPLLCSaved = FALSE;
    }

    /* Do not set the memory config for primary cards as it
       should be correct already. Only on little endian architectures
       since we need to modify the byteswap bit. -ReneR */
#if X_BYTE_ORDER == X_BIG_ENDIAN
    optionMask = OPTION1_MASK;
#else
    optionMask = (pMga->Primary) ? OPTION1_MASK_PRIMARY : OPTION1_MASK;
#endif

    /*
     * Code is needed to get things back to bank zero.
     */

    /* restore DAC registers
     * according to the docs we shouldn't write to reserved regs
     */
    for (i = 0; i < sizeof(state->DacRegs); i++) {
        if ((i <= 0x03) ||
                (i == 0x07) ||
                (i == 0x0b) ||
                (i == 0x0f) ||
                ((i >= 0x13) && (i <= 0x17)) ||
                (i == 0x1b) ||
                (i == 0x1c) ||
                ((i >= 0x1f) && (i <= 0x29)) ||
                ((i >= 0x30) && (i <= 0x37)) ||
                (MGAISGx50(pMga) && !state->PIXPLLCSaved &&
                 ((i == 0x2c) || (i == 0x2d) || (i == 0x2e) ||
                  (i == 0x4c) || (i == 0x4d) || (i == 0x4e))))
            continue;
        if (pMga->is_G200SE
                && ((i == 0x2C) || (i == 0x2D) || (i == 0x2E)))
            continue;
	if ((pMga->is_G200EV || pMga->is_G200WB || pMga->is_G200EH) &&
	    (i >= 0x44) && (i <= 0x4E))
	    continue;

        outMGAdac(i, state->DacRegs[i]);
    }

    if (pMga->is_G200ER)
	outMGAdac(0x90, state->Dac_Index90);

    if (!MGAISGx50(pMga)) {
        /* restore pci_option register */
#ifdef XSERVER_LIBPCIACCESS
        pci_device_cfg_write_bits(pMga->PciInfo, optionMask,
                                  state->Option, PCI_OPTION_REG);

        if (pMga->Chipset != PCI_CHIP_MGA1064)
            pci_device_cfg_write_bits(pMga->PciInfo, OPTION2_MASK,
                                      state->Option2, PCI_MGA_OPTION2);

        if (pMga->Chipset == PCI_CHIP_MGAG400 || pMga->Chipset == PCI_CHIP_MGAG550)
            pci_device_cfg_write_bits(pMga->PciInfo, OPTION3_MASK,
                                      state->Option3, PCI_MGA_OPTION3);
#else
        pciSetBitsLong(pMga->PciTag, PCI_OPTION_REG, optionMask,
                       state->Option);

        if (pMga->Chipset != PCI_CHIP_MGA1064)
            pciSetBitsLong(pMga->PciTag, PCI_MGA_OPTION2, OPTION2_MASK,
                           state->Option2);

        if (pMga->Chipset == PCI_CHIP_MGAG400 || pMga->Chipset == PCI_CHIP_MGAG550)
            pciSetBitsLong(pMga->PciTag, PCI_MGA_OPTION3, OPTION3_MASK,
                           state->Option3);
#endif
    }

    if (pMga->is_G200ER) {
       MGAG200ERPIXPLLSET(scrn, state);
    } else  if (pMga->is_G200EV) {
       MGAG200EVPIXPLLSET(scrn, state);
    } else if (pMga->is_G200WB) {
       MGAG200WBPIXPLLSET(scrn, state);
    } else if (pMga->is_G200EH) {
       MGAG200EHPIXPLLSET(scrn, state);
    }

    /* restore CRTCEXT regs */
    for (i = 0; i < 6; i++)
        OUTREG16(MGAREG_CRTCEXT_INDEX, (state->ExtVga[i] << 8) | i);

    if (pMga->is_G200ER) {
	OUTREG8(MGAREG_CRTCEXT_INDEX, 0x24);
	OUTREG8(MGAREG_CRTCEXT_DATA,  state->ExtVga_Index24);
    }

    /* This handles restoring the generic VGA registers. */
    if (pMga->is_G200SE) {
        MGAG200SERestoreMode(scrn, vga);

        if (vga_flags & VGA_SR_FONTS)
            MGAG200SERestoreFonts(scrn, vga);

	/*
	 This function optimize the Priority Request control
	 Higher HiPriLvl will reduce drawing performance
	 We need to give enough bandwith to crtc to avoid visual artifact
	*/
        if (pMga->reg_1e24 >= 0x02)
        {
            /* Calulate CRTC Priority value */
            CARD8  ucHiPriLvl;
            CARD32 ulBitsPerPixel;
            CARD32 ulMemoryBandwidth;

            /* uiBitsPerPixel can only be 8,16 or32 */
            if (scrn->bitsPerPixel > 16)
            {
                ulBitsPerPixel = 32;
            }
            else if (scrn->bitsPerPixel >  8)
            {
                ulBitsPerPixel = 16;
            }
            else
            {
                ulBitsPerPixel = 8;
            }

            ulMemoryBandwidth = (state->clock * ulBitsPerPixel) / 1000;

            if      (ulMemoryBandwidth    > 3100)  ucHiPriLvl = 0;
            else if (ulMemoryBandwidth    > 2600)  ucHiPriLvl = 1;
            else if (ulMemoryBandwidth    > 1900)  ucHiPriLvl = 2;
            else if (ulMemoryBandwidth    > 1160)  ucHiPriLvl = 3;
            else if (ulMemoryBandwidth    > 440)   ucHiPriLvl = 4;
            else ucHiPriLvl = 5;

            OUTREG8(0x1FDE, 0x06);
		    OUTREG8(0x1FDF, ucHiPriLvl);

            xf86DrvMsg(scrn->scrnIndex, X_INFO, "Clock           == %d\n",   state->clock);
            xf86DrvMsg(scrn->scrnIndex, X_INFO, "BitsPerPixel    == %d\n",   scrn->bitsPerPixel);
            xf86DrvMsg(scrn->scrnIndex, X_INFO, "MemoryBandwidth == %d\n",   ulMemoryBandwidth);
            xf86DrvMsg(scrn->scrnIndex, X_INFO, "HiPriLvl        == %02X\n", ucHiPriLvl);
        }
        else
        {
                xf86DrvMsg(scrn->scrnIndex, X_INFO, "Clock           == %d\n",   state->clock);
                xf86DrvMsg(scrn->scrnIndex, X_INFO, "BitsPerPixel    == %d\n",   scrn->bitsPerPixel);
                OUTREG8(0x1FDE, 0x06);
		if (pMga->reg_1e24 >= 0x01)
                {
		            OUTREG8(0x1FDF, 0x03);
                    xf86DrvMsg(scrn->scrnIndex, X_INFO, "HiPriLvl        == 03\n");
                }
		else
                {
		            OUTREG8(0x1FDF, 0x14);
                    xf86DrvMsg(scrn->scrnIndex, X_INFO, "HiPriLvl        == 14h\n");
                }
        }
    } else
        vgaHWRestore(scrn, vga, vga_flags & ~VGA_SR_CMAP);

    if (vga_flags & VGA_SR_CMAP)
        MGAGRestorePalette(scrn, vga->DAC);

    if (pMga->is_G200EV) {
	OUTREG16(MGAREG_CRTCEXT_INDEX, 6);
	OUTREG16(MGAREG_CRTCEXT_DATA, 0);
    }

    /*
     * this is needed to properly restore start address
     */
    OUTREG16(MGAREG_CRTCEXT_INDEX, (state->ExtVga[0] << 8) | 0);

    if (pMga->is_G200WB)
	MGAG200WBRestoreFromModeSwitch(scrn);

#if 1
    ErrorF("Setting DAC:");
    for (i = 0; i < sizeof(state->DacRegs); i++) {
#if 1
        if(!(i%16)) ErrorF("\n%02X: ",i);
        ErrorF("%02X ", state->DacRegs[i]);
#else
        if(!(i%8)) ErrorF("\n%02X: ",i);
        ErrorF("0x%02X, ", state->DacRegs[i]);
#endif
    }
    ErrorF("\nOPTION  = %08lX\n", state->Option);
    ErrorF("OPTION2 = %08lX\n", state->Option2);
    ErrorF("CRTCEXT:");
    for (i=0; i<6; i++) ErrorF(" %02X", state->ExtVga[i]);
    ErrorF("\n");
#endif
}

static void
state_save(xf86CrtcPtr crtc, MgaCrtcStatePtr state, int vga_flags)
{
    ScrnInfoPtr scrn = crtc->scrn;
    MGAPtr pMga = MGAPTR(scrn);
    vgaRegPtr vga = &VGAHWPTR(scrn)->SavedReg;
    int i;

    if (MGAISGx50(pMga))
        state->clock = MGAG450SavePLLFreq(scrn);

    /*
     * Code is needed to get back to bank zero.
     */
    OUTREG16(MGAREG_CRTCEXT_INDEX, 0x0004);

    /*
     * This function will handle creating the data structure and filling
     * in the generic VGA portion.
     */
    if (pMga->is_G200SE) {
        MGAG200SESaveMode(scrn, vga);

        if (vga_flags & VGA_SR_FONTS)
            MGAG200SESaveFonts(scrn, vga);
    } else
        vgaHWSave(scrn, vga, vga_flags);

    MGAGSavePalette(scrn, vga->DAC);

    /*
     * The port I/O code necessary to read in the extended registers.
     */
    for (i = 0; i < sizeof(state->DacRegs); i++)
        state->DacRegs[i] = inMGAdac(i);

    if (pMga->is_G200WB) {
	state->PllM = inMGAdac(MGA1064_WB_PIX_PLLC_M);
	state->PllN = inMGAdac(MGA1064_WB_PIX_PLLC_N);
	state->PllP = inMGAdac(MGA1064_WB_PIX_PLLC_P);
    } else if (pMga->is_G200EV) {
	state->PllM = inMGAdac(MGA1064_EV_PIX_PLLC_M);
	state->PllN = inMGAdac(MGA1064_EV_PIX_PLLC_N);
	state->PllP = inMGAdac(MGA1064_EV_PIX_PLLC_P);
    } else if (pMga->is_G200EH) {
	state->PllM = inMGAdac(MGA1064_EH_PIX_PLLC_M);
	state->PllN = inMGAdac(MGA1064_EH_PIX_PLLC_N);
	state->PllP = inMGAdac(MGA1064_EH_PIX_PLLC_P);
    } else if (pMga->is_G200ER) {
	state->PllM = inMGAdac(MGA1064_ER_PIX_PLLC_M);
	state->PllN = inMGAdac(MGA1064_ER_PIX_PLLC_N);
	state->PllP = inMGAdac(MGA1064_ER_PIX_PLLC_P);
	state->Dac_Index90 = inMGAdac(0x90);
    }

    state->PIXPLLCSaved = TRUE;

#ifdef XSERVER_LIBPCIACCESS
    pci_device_cfg_read_u32(pMga->PciInfo, &state->Option,
                            PCI_OPTION_REG);
    pci_device_cfg_read_u32(pMga->PciInfo, &state->Option2,
                            PCI_MGA_OPTION2);

    if (pMga->Chipset == PCI_CHIP_MGAG400 || pMga->Chipset == PCI_CHIP_MGAG550)
        pci_device_cfg_read_u32(pMga->PciInfo, &state->Option3,
                                PCI_MGA_OPTION3);
#else
    state->Option = pciReadLong(pMga->PciTag, PCI_OPTION_REG);
    state->Option2 = pciReadLong(pMga->PciTag, PCI_MGA_OPTION2);

    if (pMga->Chipset == PCI_CHIP_MGAG400 || pMga->Chipset == PCI_CHIP_MGAG550)
        state->Option3 = pciReadLong(pMga->PciTag, PCI_MGA_OPTION3);
#endif

    for (i = 0; i < 6; i++) {
        OUTREG8(MGAREG_CRTCEXT_INDEX, i);
        state->ExtVga[i] = INREG8(MGAREG_CRTCEXT_DATA);
    }
    if (pMga->is_G200ER)
    {
	OUTREG8(MGAREG_CRTCEXT_INDEX, 0x24);
	state->ExtVga_Index24 = INREG8(MGAREG_CRTCEXT_DATA);
    }

#ifdef DEBUG
    ErrorF("Saved values:\nDAC:");
    for (i = 0; i < sizeof(state->DacRegs); i++) {
#if 1
        if(!(i%16)) ErrorF("\n%02X: ",i);
        ErrorF("%02X ", state->DacRegs[i]);
#else
        if(!(i%8)) ErrorF("\n%02X: ",i);
        ErrorF("0x%02X, ", state->DacRegs[i]);
#endif
    }

    ErrorF("\nOPTION  = %08lX\n:", state->Option);
    ErrorF("OPTION2 = %08lX\nCRTCEXT:", state->Option2);

    for (i=0; i<6; i++) ErrorF(" %02X", state->ExtVga[i]);
        ErrorF("\n");
#endif
}

static void
crtc_save(xf86CrtcPtr crtc)
{
    MgaCrtcDataPtr data = MGACRTCDATAPTR(crtc);
    MGAPtr pMga = MGAPTR(crtc->scrn);
    int vga_flags = VGA_SR_MODE;

    if (pMga->Primary)
        vga_flags |= VGA_SR_FONTS;

    state_save(crtc, &data->saved_state, vga_flags);
}

static void
crtc_restore(xf86CrtcPtr crtc)
{
    MgaCrtcDataPtr data = MGACRTCDATAPTR(crtc);
    MGAPtr pMga = MGAPTR(crtc->scrn);
    vgaHWPtr vga = VGAHWPTR(crtc->scrn);
    int vga_flags = VGA_SR_MODE | VGA_SR_CMAP;

    if (pMga->Primary)
        vga_flags |= VGA_SR_FONTS;

    state_restore(crtc, &data->saved_state, &vga->SavedReg, vga_flags);
}

static Bool
crtc_lock(xf86CrtcPtr crtc)
{
    /* XXX this would need DRI support */
    return FALSE;
}

static void
crtc_unlock(xf86CrtcPtr crtc)
{
    /* XXX this would need DRI support */
}

static Bool
crtc_mode_fixup(xf86CrtcPtr crtc, DisplayModePtr mode,
                DisplayModePtr adjusted_mode)
{
    return TRUE;
}

static void
crtc_prepare(xf86CrtcPtr crtc)
{
    crtc->funcs->dpms(crtc, DPMSModeOff);
}

static void
crtc_mode_set(xf86CrtcPtr crtc, DisplayModePtr mode,
              DisplayModePtr adjust_mode, int x, int y)
{
    MgaCrtcStateRec state;
    vgaHWPtr vga = VGAHWPTR(crtc->scrn);

    memset(&state, 0, sizeof (state));

    state_set(crtc, &state, mode, x, y);
    state_restore(crtc, &state, &vga->ModeReg, VGA_SR_MODE);
}

static void
crtc_commit(xf86CrtcPtr crtc)
{
    crtc->funcs->dpms(crtc, DPMSModeOn);
}

static void
crtc_destroy(xf86CrtcPtr crtc)
{
    free(crtc->driver_private);
}

Bool
MGAGCrtc1Init(ScrnInfoPtr scrn)
{
    xf86CrtcPtr crtc;
    MgaCrtcDataPtr data;

    data = xnfcalloc (sizeof (MgaCrtcDataRec), 1);
    if (!data)
        return FALSE;

    crtc = xf86CrtcCreate (scrn, &crtc_funcs);
    if (!crtc) {
        free(data);
        return FALSE;
    }

    crtc->driver_private = data;

    return TRUE;
}
