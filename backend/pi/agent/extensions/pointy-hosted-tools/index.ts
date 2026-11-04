import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const hostedToolNames = (process.env.POINTY_HOSTED_TOOLS ?? "").split(",").filter((name) => name !== "");

// Neuralwatt runs a hosted tool that a Chat Completions request names with a function entry holding only its name.
// https://docs.neuralwatt.com/hosted-tools/overview
const hostedToolEntries = hostedToolNames.map((name) => ({ type: "function", function: { name } }));

export default function (pi: ExtensionAPI) {
	if (hostedToolEntries.length === 0) return;
	pi.on("before_provider_request", (event) => {
		const payload = event.payload as { tools?: unknown[] };
		if (!Array.isArray(payload.tools)) return undefined;
		return { ...payload, tools: [...payload.tools, ...hostedToolEntries] };
	});
}
