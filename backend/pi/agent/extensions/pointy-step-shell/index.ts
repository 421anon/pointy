import { readFileSync } from "node:fs";
import { Type } from "@earendil-works/pi-ai";
import { defineTool, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

const stepShellScript = readFileSync(new URL("./step-shell.sh", import.meta.url), "utf8");
const stepInputsExpression = readFileSync(new URL("./step-inputs.nix", import.meta.url), "utf8");

const shellQuote = (text: string) => `'${text.replaceAll("'", `'\\''`)}'`;

const stepShellTool = defineTool({
	name: "step_shell",
	label: "Step shell",
	description:
		"Run a bash command in a step's build environment, to check its script or SQL on small inputs before the user runs the step. The command runs in $HOME/steps/<step>/, laid out like the step's build directory: the step's srcFiles linked from the worktree, so your edits apply at once, each upstream step's output linked as <step id>/, and an empty out/. PATH holds the tools the step's build has: its interpreters with their packages, its Nix dependencies, coreutils, gawk, sed and grep. Files you add there persist between calls. It never builds an upstream step: when one has no output yet, it says so and runs nothing.",
	parameters: Type.Object({
		step: Type.Integer({ minimum: 0, description: "Step id; the step may be one you have not applied yet" }),
		command: Type.String({ description: "Bash command to run in the step's build environment" }),
		timeout: Type.Optional(Type.Number({ description: "Timeout in seconds, including about 5 seconds of setup" })),
	}),

	async execute(_toolCallId, params, signal, onUpdate, ctx) {
		const command = ["bash", "-c", stepShellScript, "step_shell", String(params.step), params.command, stepInputsExpression]
			.map(shellQuote)
			.join(" ");
		const bashParams = params.timeout === undefined ? { command } : { command, timeout: params.timeout };
		const { result, isError } = await ctx.executeTool("bash", bashParams, { signal, onUpdate });
		return { content: result.content, details: result.details, isError };
	},
});

export default function (pi: ExtensionAPI) {
	pi.registerTool(stepShellTool);
}
