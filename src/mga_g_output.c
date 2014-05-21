/*
 * Copyright 2007 Tilman Sauerbeck
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
 *
 * Sources:
 *    xf86-video-intel
 */

#ifdef HAVE_CONFIG_H
# include "config.h"
#endif

#include "xf86.h"

#include "mga_reg.h"
#include "mga.h"
#include "mga_randr.h"
#include "vbe.h"

#define MGAOUTPUTDATAPTR(p) ((MgaOutputDataPtr) ((p)->driver_private))

typedef struct {
    I2CBusPtr ddc_bus;
} MgaOutputDataRec, *MgaOutputDataPtr;

static void output_dac1_dpms(xf86OutputPtr output, int mode);
static void output_save(xf86OutputPtr output);
static void output_restore(xf86OutputPtr output);
static int output_mode_valid(xf86OutputPtr output, DisplayModePtr mode);
static Bool output_mode_fixup(xf86OutputPtr output, DisplayModePtr mode,
                              DisplayModePtr adjusted_mode);
static void output_prepare(xf86OutputPtr output);
static void output_mode_set(xf86OutputPtr output, DisplayModePtr mode,
                            DisplayModePtr adjusted_mode);
static void output_commit(xf86OutputPtr output);
static xf86OutputStatus output_detect(xf86OutputPtr output);
static DisplayModePtr output_get_modes(xf86OutputPtr output);
static void output_destroy(xf86OutputPtr output);

static const xf86OutputFuncsRec output_dac1_funcs = {
    .dpms = output_dac1_dpms,
    .save = output_save,
    .restore = output_restore,
    .mode_valid = output_mode_valid,
    .mode_fixup = output_mode_fixup,
    .prepare = output_prepare,
    .mode_set = output_mode_set,
    .commit = output_commit,
    .detect = output_detect,
    .get_modes = output_get_modes,
    .destroy = output_destroy
};

/* enable/disable primary output. */
static void
output1_dpms(xf86OutputPtr output, int mode)
{
    MGAPtr pMga = MGAPTR(output->scrn);
    CARD8 misc_ctl, disp_ctl, mask;

    misc_ctl = inMGAdac(MGA1064_MISC_CTL);
    mask = MGA1064_MISC_CTL_DAC_EN;

    if (mode == DPMSModeOn)
        outMGAdac(MGA1064_MISC_CTL, misc_ctl | mask);
    else
        outMGAdac(MGA1064_MISC_CTL, misc_ctl & ~mask);

    disp_ctl = inMGAdac(MGA1064_DISP_CTL);
    mask = MGA1064_DISP_CTL_DAC1OUTSEL_EN;

    if (mode == DPMSModeOn)
        outMGAdac(MGA1064_DISP_CTL, disp_ctl | mask);
    else
        outMGAdac(MGA1064_DISP_CTL, disp_ctl & ~mask);
}

static void
output_dac1_dpms(xf86OutputPtr output, int mode)
{
    /* XXX
     * . Prefer an implementation that doesn't depend on VGA specifics.
     *
     * . This will only work for the primary output or maybe only for
     *   CRTC1?
     */

    MGAPtr pMga = MGAPTR(output->scrn);

#if 0
    CARD8 val, seq1, crtcext1;

    OUTREG8(MGAREG_SEQ_INDEX, 0x01); /* Select SEQ1 */
    OUTREG8(MGAREG_CRTCEXT_INDEX, 0x01); /* Select CRTCEXT1 */

    seq1 = INREG8(MGAREG_SEQ_DATA);
    seq1 &= ~MGAREG_SEQ1_SCREEN_OFF;

    crtcext1 = INREG8(MGAREG_CRTCEXT_DATA);
    crtcext1 &= ~MGAREG_CRTCEXT1_HSYNC_OFF;
    crtcext1 &= ~MGAREG_CRTCEXT1_VSYNC_OFF;

    switch (mode) {
    case DPMSModeOn:
        /* nothing to do */
        break;
    case DPMSModeStandby:
        seq1 |= MGAREG_SEQ1_SCREEN_OFF;
        crtcext1 = MGAREG_CRTCEXT1_HSYNC_OFF;
        break;
    case DPMSModeSuspend:
        seq1 |= MGAREG_SEQ1_SCREEN_OFF;
        crtcext1 |= MGAREG_CRTCEXT1_VSYNC_OFF;
        break;
    case DPMSModeOff:
        seq1 |= MGAREG_SEQ1_SCREEN_OFF;
        crtcext1 |= MGAREG_CRTCEXT1_HSYNC_OFF;
        crtcext1 |= MGAREG_CRTCEXT1_VSYNC_OFF;
        break;
    }

    MGAWAITVSYNC();
    MGAWAITBUSY();

    OUTREG8(MGAREG_SEQ_DATA, seq1);

    usleep(20000);

    OUTREG8(MGAREG_CRTCEXT_DATA, crtcext1);
#endif

#if 0
    /* this is wrong on at least EV and EH */
    output1_dpms(output, mode);
#endif

    MGADisplayPowerManagementSet(output->scrn, mode, 0);
}

static void
output_save(xf86OutputPtr output)
{
}

static void
output_restore(xf86OutputPtr output)
{
}

static int
output_mode_valid(xf86OutputPtr output, DisplayModePtr mode)
{
    return mga_valid_mode(output->scrn, mode, 0);
}

static Bool
output_mode_fixup(xf86OutputPtr output, DisplayModePtr mode,
                   DisplayModePtr adjusted_mode)
{
    return TRUE;
}

static void
output_prepare(xf86OutputPtr output)
{
    output->funcs->dpms(output, DPMSModeOff);
}

static void
output_mode_set(xf86OutputPtr output, DisplayModePtr mode,
                DisplayModePtr adjusted_mode)
{
}

static void
output_commit(xf86OutputPtr output)
{
    output->funcs->dpms(output, DPMSModeOn);
}

static xf86OutputStatus
output_detect(xf86OutputPtr output)
{
    MgaOutputDataPtr data = MGAOUTPUTDATAPTR(output);

    /* lie lie lie */
    return XF86OutputStatusConnected;

    /* 0xa0 is DDC EEPROM address */
    if (xf86I2CProbeAddress(data->ddc_bus, 0xa0))
        return XF86OutputStatusConnected;
    else
        return XF86OutputStatusUnknown;
}

/* jesus christ i hate everything */
static vbeInfoPtr vbe;

static DisplayModePtr
output_get_modes(xf86OutputPtr output)
{
    MgaOutputDataPtr data = MGAOUTPUTDATAPTR(output);
    xf86MonPtr mon;

#if 1
    if (!vbe) {
	xf86LoadSubModule(output->scrn, "vbe");
	vbe = VBEInit(NULL, MGAPTR(output->scrn)->pEnt->index);
    }

    /* mon = xf86OutputGetEDID(output, data->ddc_bus); */
    mon = vbeDoEDID(vbe, NULL);
#else

    extern xf86MonPtr MGAdoDDC(ScrnInfoPtr pScrn);
    mon = MGAdoDDC(output->scrn);
#endif

    xf86OutputSetEDID(output, mon);

    return xf86OutputGetEDIDModes(output);
}

static void
output_destroy(xf86OutputPtr output)
{
    free(output->driver_private);
}

xf86OutputPtr
MGAG200EOutputInit(ScrnInfoPtr scrn)
{
    MGAPtr pMga = MGAPTR(scrn);
    xf86OutputPtr output;
    MgaOutputDataPtr data;

    data = xnfcalloc(sizeof(MgaOutputDataRec), 1);
    if (!data)
        return NULL;

    output = xf86OutputCreate(scrn, &output_dac1_funcs, "VGA1");
    if (!output) {
        free(data);
        return NULL;
    }

    output->driver_private = data;

    data->ddc_bus = pMga->DDC_Bus1;

    return output;
}
