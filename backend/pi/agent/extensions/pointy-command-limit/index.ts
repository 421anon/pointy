import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const defaultTimeoutSeconds = Number(process.env.PI_COMMAND_TIMEOUT_SECONDS ?? 120);

export default function (pi: ExtensionAPI) {
	pi.on("tool_call", (event) => {
		if (event.toolName !== "bash") return undefined;
		const input = event.input as { command?: unknown; timeout?: unknown };
		if (typeof input.command !== "string") return undefined;
		if (typeof input.timeout !== "number") {
			input.timeout = defaultTimeoutSeconds;
		}
		return undefined;
	});
}
