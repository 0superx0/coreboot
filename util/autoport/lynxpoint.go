package main

import (
	"fmt"
	"os"
)

type lynxpoint struct {
	variant string
	node    *DevTreeNode
}

func (b lynxpoint) writeGPIOSet(ctx Context, sb *os.File,
	val uint32, set uint, partno int, constraint uint32) {

	max := uint(32)
	if set == 3 {
		max = 12
	}

	bits := [6][2]string{
		{"GPIO_MODE_NATIVE", "GPIO_MODE_GPIO"},
		{"GPIO_DIR_OUTPUT", "GPIO_DIR_INPUT"},
		{"GPIO_LEVEL_LOW", "GPIO_LEVEL_HIGH"},
		{"GPIO_RESET_PWROK", "GPIO_RESET_RSMRST"},
		{"GPIO_NO_INVERT", "GPIO_INVERT"},
		{"GPIO_NO_BLINK", "GPIO_BLINK"},
	}

	for i := uint(0); i < max; i++ {
		if ((constraint>>i)&1 == 1) {
			fmt.Fprintf(sb, "	.gpio%d = %s,\n",
				(set-1)*32+i,
				bits[partno][(val>>i)&1])
		}
	}
}

func (b lynxpoint) GPIO(ctx Context, inteltool InteltoolData) {
	var constraint uint32
	gpio := Create(ctx, "gpio.c")
	defer gpio.Close()

	AddROMStageFile("gpio.c", "")

	Add_gpl(gpio)
	gpio.WriteString("#include <southbridge/intel/common/gpio.h>\n\n")

	addresses := [3][6]int{
		{0x00, 0x04, 0x0c, 0x60, 0x2c, 0x18},
		{0x30, 0x34, 0x38, 0x64, -1, -1},
		{0x40, 0x44, 0x48, 0x68, -1, -1},
	}

	for set := 1; set <= 3; set++ {
		for partno, part := range []string{"mode", "direction", "level", "reset", "invert", "blink"} {
			addr := addresses[set-1][partno]
			if addr < 0 {
				continue
			}
			fmt.Fprintf(gpio, "static const struct pch_gpio_set%d pch_gpio_set%d_%s = {\n",
				set, set, part)

			constraint = 0xffffffff
			switch part {
			case "direction":
				/* Ignored on native mode */
				constraint = inteltool.GPIO[uint16(addresses[set-1][0])]
			case "level":
				/* Level doesn't matter for input */
				constraint = inteltool.GPIO[uint16(addresses[set-1][0])]
				constraint &^= inteltool.GPIO[uint16(addresses[set-1][1])]
			case "reset":
				/* Only show reset */
				constraint = inteltool.GPIO[uint16(addresses[set-1][3])]
			case "invert":
				/* Only on input and only show inverted GPIO */
				constraint = inteltool.GPIO[uint16(addresses[set-1][0])]
				constraint &= inteltool.GPIO[uint16(addresses[set-1][1])]
				constraint &= inteltool.GPIO[uint16(addresses[set-1][4])]
			case "blink":
				/* Only on output and only show blinking GPIO */
				constraint = inteltool.GPIO[uint16(addresses[set-1][0])]
				constraint &^= inteltool.GPIO[uint16(addresses[set-1][1])]
				constraint &= inteltool.GPIO[uint16(addresses[set-1][5])]
			}
			b.writeGPIOSet(ctx, gpio, inteltool.GPIO[uint16(addr)], uint(set), partno, constraint)
			gpio.WriteString("};\n\n")
		}
	}

	gpio.WriteString(`const struct pch_gpio_map mainboard_gpio_map = {
	.set1 = {
		.mode		= &pch_gpio_set1_mode,
		.direction	= &pch_gpio_set1_direction,
		.level		= &pch_gpio_set1_level,
		.blink		= &pch_gpio_set1_blink,
		.invert		= &pch_gpio_set1_invert,
		.reset		= &pch_gpio_set1_reset,
	},
	.set2 = {
		.mode		= &pch_gpio_set2_mode,
		.direction	= &pch_gpio_set2_direction,
		.level		= &pch_gpio_set2_level,
		.reset		= &pch_gpio_set2_reset,
	},
	.set3 = {
		.mode		= &pch_gpio_set3_mode,
		.direction	= &pch_gpio_set3_direction,
		.level		= &pch_gpio_set3_level,
		.reset		= &pch_gpio_set3_reset,
	},
};
`)
}

func (b lynxpoint) IsPCIeHotplug(ctx Context, port int) bool {
	portDev, ok := PCIMap[PCIAddr{Bus: 0, Dev: 0x1c, Func: port}]
	if !ok {
		return false
	}
	return (portDev.ConfigDump[0xdb] & (1 << 6)) != 0
}

func (b lynxpoint) GetGPIOHeader() string {
	return "southbridge/intel/lynxpoint/pch.h"
}

func (b lynxpoint) EnableGPE(in int) {
	b.node.Registers[fmt.Sprintf("gpi%d_routing", in)] = "2"
}

func (b lynxpoint) EncodeGPE(in int) int {
	return in + 0x10
}

func (b lynxpoint) DecodeGPE(in int) int {
	return in - 0x10
}

func (b lynxpoint) NeedRouteGPIOManually() {
	b.node.Comment += ", FIXME: set gpiX_routing for EC support"
}

func (b lynxpoint) Scan(ctx Context, addr PCIDevData) {

	SouthBridge = &b

	inteltool := ctx.InfoSource.GetInteltool()
	b.GPIO(ctx, inteltool)

	KconfigBool["SOUTHBRIDGE_INTEL_"+b.variant] = true
	KconfigBool["SERIRQ_CONTINUOUS_MODE"] = true
	KconfigInt["USBDEBUG_HCD_INDEX"] = 2
	KconfigComment["USBDEBUG_HCD_INDEX"] = "FIXME: check this"
	dmi := ctx.InfoSource.GetDMI()
	if dmi.Vendor == "LENOVO" {
		KconfigInt["DRAM_RESET_GATE_GPIO"] = 10
	} else {
		KconfigInt["DRAM_RESET_GATE_GPIO"] = 60
	}
	KconfigComment["DRAM_RESET_GATE_GPIO"] = "FIXME: check this"

	/* Not strictly speaking correct. These subsys/subvendor referer to PCI devices.
	   But most systems don't have any of those. But the config needs to be set
	   nevertheless. So set it to southbridge subsys/subvendor.  */
	KconfigHex["MAINBOARD_PCI_SUBSYSTEM_VENDOR_ID"] = uint32(GetLE16(addr.ConfigDump[0x2c:0x2e]))
	KconfigHex["MAINBOARD_PCI_SUBSYSTEM_DEVICE_ID"] = uint32(GetLE16(addr.ConfigDump[0x2e:0x30]))

	ich9GetFlashSize(ctx)

	DSDTDefines = append(DSDTDefines,
		DSDTDefine{
			Key:   "BRIGHTNESS_UP",
			Value: "\\_SB.PCI0.GFX0.INCB",
		},
		DSDTDefine{
			Key:   "BRIGHTNESS_DOWN",
			Value: "\\_SB.PCI0.GFX0.DECB",
		},
		DSDTDefine{
			Key:   "ACPI_VIDEO_DEVICE",
			Value: "\\_SB.PCI0.GFX0",
		})

	/* SPI init */
	MainboardIncludes = append(MainboardIncludes, "southbridge/intel/lynxpoint/pch.h")

	FADT := ctx.InfoSource.GetACPI()["FACP"]

	pcieHotplugMap := "{ "

	for port := 0; port < 7; port++ {
		if b.IsPCIeHotplug(ctx, port) {
			pcieHotplugMap += "1, "
		} else {
			pcieHotplugMap += "0, "
		}
	}

	if b.IsPCIeHotplug(ctx, 7) {
		pcieHotplugMap += "1 }"
	} else {
		pcieHotplugMap += "0 }"
	}

	cur := DevTreeNode{
		Chip:    "southbridge/intel/lynxpoint",
		Comment: "Intel Series 6 Cougar Point PCH",

		Registers: map[string]string{
			"sata_interface_speed_support": "0x3",
			"gen1_dec":                     FormatHexLE32(PCIMap[PCIAddr{Bus: 0, Dev: 0x1f, Func: 0}].ConfigDump[0x84:0x88]),
			"gen2_dec":                     FormatHexLE32(PCIMap[PCIAddr{Bus: 0, Dev: 0x1f, Func: 0}].ConfigDump[0x88:0x8c]),
			"gen3_dec":                     FormatHexLE32(PCIMap[PCIAddr{Bus: 0, Dev: 0x1f, Func: 0}].ConfigDump[0x8c:0x90]),
			"gen4_dec":                     FormatHexLE32(PCIMap[PCIAddr{Bus: 0, Dev: 0x1f, Func: 0}].ConfigDump[0x90:0x94]),
			"pcie_port_coalesce":           "1",
			"pcie_hotplug_map":             pcieHotplugMap,

			"sata_port_map": fmt.Sprintf("0x%x", PCIMap[PCIAddr{Bus: 0, Dev: 0x1f, Func: 2}].ConfigDump[0x92]&0x3f),

			"p_cnt_throttling_supported": (FormatBool(FADT[104] == 1 && FADT[105] == 3)),
			"c2_latency":                 FormatHexLE16(FADT[96:98]),
			"docking_supported":          (FormatBool((FADT[113] & (1 << 1)) != 0)),
			"spi_uvscc": fmt.Sprintf("0x%x", inteltool.RCBA[0x38c8]),
			"spi_lvscc": fmt.Sprintf("0x%x", inteltool.RCBA[0x38c4] &^ (1 << 23)),
		},
		PCISlots: []PCISlot{
			PCISlot{PCIAddr: PCIAddr{Dev: 0x14, Func: 0}, writeEmpty: false, additionalComment: "USB 3.0 Controller"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x16, Func: 0}, writeEmpty: true, additionalComment: "Management Engine Interface 1"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x16, Func: 1}, writeEmpty: true, additionalComment: "Management Engine Interface 2"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x16, Func: 2}, writeEmpty: true, additionalComment: "Management Engine IDE-R"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x16, Func: 3}, writeEmpty: true, additionalComment: "Management Engine KT"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x19, Func: 0}, writeEmpty: true, additionalComment: "Intel Gigabit Ethernet"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1a, Func: 0}, writeEmpty: true, additionalComment: "USB2 EHCI #2"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1b, Func: 0}, writeEmpty: true, additionalComment: "High Definition Audio"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1c, Func: 0}, writeEmpty: true, additionalComment: "PCIe Port #1"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1c, Func: 1}, writeEmpty: true, additionalComment: "PCIe Port #2"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1c, Func: 2}, writeEmpty: true, additionalComment: "PCIe Port #3"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1c, Func: 3}, writeEmpty: true, additionalComment: "PCIe Port #4"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1c, Func: 4}, writeEmpty: true, additionalComment: "PCIe Port #5"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1c, Func: 5}, writeEmpty: true, additionalComment: "PCIe Port #6"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1c, Func: 6}, writeEmpty: true, additionalComment: "PCIe Port #7"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1c, Func: 7}, writeEmpty: true, additionalComment: "PCIe Port #8"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1d, Func: 0}, writeEmpty: true, additionalComment: "USB2 EHCI #1"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1e, Func: 0}, writeEmpty: true, additionalComment: "PCI bridge"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1f, Func: 0}, writeEmpty: true, additionalComment: "LPC bridge"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1f, Func: 2}, writeEmpty: true, additionalComment: "SATA Controller 1"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1f, Func: 3}, writeEmpty: true, additionalComment: "SMBus"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1f, Func: 5}, writeEmpty: true, additionalComment: "SATA Controller 2"},
			PCISlot{PCIAddr: PCIAddr{Dev: 0x1f, Func: 6}, writeEmpty: true, additionalComment: "Thermal"},
		},
	}

	b.node = &cur

	xhciDev, ok := PCIMap[PCIAddr{Bus: 0, Dev: 0x14, Func: 0}]

	if ok {
		cur.Registers["xhci_switchable_ports"] = FormatHexLE32(xhciDev.ConfigDump[0xd4:0xd8])
		cur.Registers["superspeed_capable_ports"] = FormatHexLE32(xhciDev.ConfigDump[0xdc:0xe0])
		cur.Registers["xhci_overcurrent_mapping"] = FormatHexLE32(xhciDev.ConfigDump[0xc0:0xc4])
	}

	PutPCIChip(addr, cur)
	PutPCIDevParent(addr, "PCI-LPC bridge", "lpc")

	DSDTIncludes = append(DSDTIncludes, DSDTInclude{
		File: "southbridge/intel/lynxpoint/acpi/platform.asl",
	})
	DSDTIncludes = append(DSDTIncludes, DSDTInclude{
		File:    "southbridge/intel/lynxpoint/acpi/globalnvs.asl",
		Comment: "global NVS and variables",
	})
	DSDTIncludes = append(DSDTIncludes, DSDTInclude{
		File: "southbridge/intel/lynxpoint/acpi/sleepstates.asl",
	})
	DSDTPCI0Includes = append(DSDTPCI0Includes, DSDTInclude{
		File: "southbridge/intel/lynxpoint/acpi/pch.asl",
	})

	sb := Create(ctx, "romstage.c")
	defer sb.Close()
	Add_gpl(sb)
	sb.WriteString(`#include <stdint.h>
#include <cpu/intel/romstage.h>
#include <cpu/intel/haswell/haswell.h>
#include <northbridge/intel/haswell/haswell.h>
#include <northbridge/intel/haswell/pei_data.h>
#include <southbridge/intel/common/gpio.h>
#include <southbridge/intel/lynxpoint/pch.h>

static const struct rcba_config_instruction rcba_config[] = {
`)
	RestoreDIRRoute(sb, "D31IR", uint16(inteltool.RCBA[0x3140]))
	RestoreDIRRoute(sb, "D29IR", uint16(inteltool.RCBA[0x3144]))
	RestoreDIRRoute(sb, "D28IR", uint16(inteltool.RCBA[0x3146]))
	RestoreDIRRoute(sb, "D27IR", uint16(inteltool.RCBA[0x3148]))
	RestoreDIRRoute(sb, "D26IR", uint16(inteltool.RCBA[0x314c]))
	RestoreDIRRoute(sb, "D25IR", uint16(inteltool.RCBA[0x3150]))
	RestoreDIRRoute(sb, "D22IR", uint16(inteltool.RCBA[0x315c]))
	RestoreDIRRoute(sb, "D20IR", uint16(inteltool.RCBA[0x3160]))

	sb.WriteString(`
	RCBA_RMW_REG_32(FD, ~0, PCH_DISABLE_ALWAYS),

	RCBA_END_CONFIG,
};`)

	sb.WriteString(`
void pch_enable_lpc(void)
{
`)
	RestorePCI16Simple(sb, addr, 0x82)
	RestorePCI32Simple(sb, addr, 0x84)
	RestorePCI32Simple(sb, addr, 0x88)
	RestorePCI32Simple(sb, addr, 0x8c)
	RestorePCI32Simple(sb, addr, 0x90)

	RestorePCI16Simple(sb, addr, 0x80)

	sb.WriteString(`}

void mainboard_rcba_config(void)
{
`)
	sb.WriteString("}\n\n")

	sb.WriteString("const struct southbridge_usb_port mainboard_usb_ports[] = {\n")

	currentMap := map[uint32]int{
		0x20000153: 0,
		0x20000f57: 1,
		0x2000055b: 2,
		0x20000f51: 3,
		0x2000094a: 4,
	}

	for port := uint(0); port < 14; port++ {
		var pinmask uint32
		OCPin := -1
		if port < 8 {
			pinmask = inteltool.RCBA[0x35a0]
		} else {
			pinmask = inteltool.RCBA[0x35a4]
		}
		for pin := uint(0); pin < 4; pin++ {
			if ((pinmask >> ((port % 8) + 8*pin)) & 1) != 0 {
				OCPin = int(pin)
				if port >= 8 {
					OCPin += 4
				}
			}
		}
		fmt.Fprintf(sb, "\t{ %d, %d, %d },\n",
			((inteltool.RCBA[0x359c]>>port)&1)^1,
			currentMap[inteltool.RCBA[uint16(0x3500+4*port)]],
			OCPin)
	}
	sb.WriteString("};\n")

	guessedMap := GuessSPDMap(ctx)

	sb.WriteString(`
void mainboard_early_init(int s3resume)
{
}

void mainboard_config_superio(void)
{
}

/* FIXME: Put proper SPD map here. */
void mainboard_get_spd(spd_raw_data *spd, bool id_only)
{
`)
	for i, spd := range guessedMap {
		fmt.Fprintf(sb, "\tread_spd(&spd[%d], 0x%02x, id_only);\n", i, spd)
	}
	sb.WriteString("}\n")

	gnvs := Create(ctx, "gnvs.c")
	defer gnvs.Close()

	Add_gpl(gnvs)
	gnvs.WriteString(`#include <southbridge/intel/lynxpoint/nvs.h>

/* FIXME: check this function.  */
void acpi_create_gnvs(global_nvs_t *gnvs)
{
	/* Disable USB ports in S3 by default */
	gnvs->s3u0 = 0;
	gnvs->s3u1 = 0;

	/* Disable USB ports in S5 by default */
	gnvs->s5u0 = 0;
	gnvs->s5u1 = 0;

	// the lid is open by default.
	gnvs->lids = 1;

	gnvs->tcrt = 100;
	gnvs->tpsv = 90;
}
`)

	AddRAMStageFile("gnvs.c", "")
}

func init() {
	for _, id := range []uint16 {
		0x8c41, 0x8c49, 0x8c4b, 0x8c4f,
	} {
		RegisterPCI(0x8086, uint16(id), lynxpoint{variant: "Lynx Point Mobile"})
	}

	for _, id := range []uint16 {
		0x8c42, 0x8c44, 0x8c46, 0x8c4a,
		0x8c4c, 0x8c4e, 0x8c50, 0x8c5c,
	} {
		RegisterPCI(0x8086, uint16(id), lynxpoint{variant: "Lynx Point Desktop"})
	}

	for _, id := range []uint16 {
		0x8c52, 0x8c54, 0x8c56,
	} {
		RegisterPCI(0x8086, uint16(id), lynxpoint{variant: "Lynx Point Server"})
	}

	for _, id := range []uint16 {
		0x9c41, 0x9c43, 0x9c45,
	} {
		RegisterPCI(0x8086, uint16(id), lynxpoint{variant: "Lynx Point LP"})
	}

	/* PCIe bridge */
	for _, id := range []uint16{
		0x8c10, 0x8c12, 0x8c14, 0x8c16, 0x8c18, 0x8c1a, 0x8c1c, 0x8c1e,
		0x9c10, 0x9c12, 0x9c14, 0x9c16, 0x9c18, 0x9c1a,
	} {
		RegisterPCI(0x8086, id, GenericPCI{})
	}

	/* SMBus controller  */
	RegisterPCI(0x8086, 0x1c22, GenericPCI{MissingParent: "smbus"})
	RegisterPCI(0x8086, 0x1e22, GenericPCI{MissingParent: "smbus"})
	RegisterPCI(0x8086, 0x8c22, GenericPCI{MissingParent: "smbus"})
	RegisterPCI(0x8086, 0x9c22, GenericPCI{MissingParent: "smbus"})

	/* SATA */
	for _, id := range []uint16{
		0x8c00, 0x8c02, 0x8c04, 0x8c06, 0x8c08, 0x8c0e,
		0x8c01, 0x8c03, 0x8c05, 0x8c07, 0x8c09, 0x8c0f,
		0x9c03, 0x9c05, 0x9c07, 0x9c0f,
	} {
		RegisterPCI(0x8086, id, GenericPCI{})
	}

	/* EHCI */
	for _, id := range []uint16{
		0x9c26, 0x8c26, 0x8c2d,
	} {
		RegisterPCI(0x8086, id, GenericPCI{})
	}

	/* XHCI */
	RegisterPCI(0x8086, 0x8c31, GenericPCI{})
	RegisterPCI(0x8086, 0x9c31, GenericPCI{})

	/* ME and children */
	for _, id := range []uint16{
		0x8c3a, 0x8c3b,
	} {
		RegisterPCI(0x8086, id, GenericPCI{})
	}

	/* Ethernet */
	RegisterPCI(0x8086, 0x8c33, GenericPCI{})
}
